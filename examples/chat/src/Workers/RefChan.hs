module Workers.RefChan where

import Codec.Serialise
import Config
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import DB
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.HashSet qualified as HS
import Env
import Error
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Data.Types.SignedBox
import HBS2.Merkle
import HBS2.Net.Auth.Credentials hiding (encode)
import HBS2.Net.Proto.Notify
import HBS2.OrDie
import HBS2.Peer.Notify
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.RPC.Client.Unix hiding (encode)
import HBS2.Prelude hiding (line)
import HBS2.Storage
import Lens.Micro.Mtl
import Message
import Streaming.Prelude qualified as S
import Types
import UnliftIO

refChanWorker :: (MonadUnliftIO m, MonadReader Env m) => m ()
refChanWorker = do
  refChans' <- asks (refChans . config)
  case refChans' of
    [] -> do
      liftIO $ putStrLn "No refchans found in the config"
      pure ()
    someRefChans -> do
      chatEventsChan' <- asks chatEventsChan
      notifyWorkers <-
        concat <$> forM someRefChans \namedRefChan -> do
          let refChan = namedRefChanKey namedRefChan
          refChanEventHandlerAsync <- async do
            sink <- asks refChanNotifySink
            runNotifySink sink (RefChanNotifyKey $ fromMyPublicKey refChan) $ \case
              RefChanHeadUpdated _refChan _oldRefChanHeadHashRef newRefChanHeadHashRef -> do
                newRefChanHead <-
                  readRefChanHead newRefChanHeadHashRef
                    >>= orThrow (ServerError "can't request refchan head")
                let refChanMembers = getRefChanHeadMembers newRefChanHead
                wsMembers <- getWSMembersFromRefChanMembers refChanMembers refChan
                atomically $
                  writeTChan chatEventsChan' $
                    MembersEvent
                      { membersEventRefChan = refChan
                      , membersEventAuthors = wsMembersAuthors wsMembers
                      , membersEventReaders = wsMembersReaders wsMembers
                      }
                pure ()
              _ -> pure ()
          refChanTxEventHandlerAsync <- async do
            sink <- asks refChanTxNotifySink
            runNotifySink sink (RefChanTxNotifyKey $ fromMyPublicKey refChan) $ \case
              RefChanTxNotifyData _refChan tx -> case unpackTx tx of
                Nothing -> pure () -- TODO: handle this properly
                Just messageHashRef -> do
                  processMessageResult <- processMessage True refChan messageHashRef
                  case processMessageResult of
                    Nothing ->
                      atomically $
                        writeTChan chatEventsChan' $
                          MessageAddedToDownloadQueueEvent $
                            MessageDownloadQueueItem
                              { messageDownloadQueueItemRefChan = refChan
                              , messageDownloadQueueItemHashRef = messageHashRef
                              }
                    Just decryptedMessage ->
                      atomically $
                        writeTChan chatEventsChan' $
                          MessageEvent
                            { messageEventRefChan = refChan
                            , messageEventMessage = decryptedMessage
                            }
          pure [refChanEventHandlerAsync, refChanTxEventHandlerAsync]
      void $ waitAnyCancel notifyWorkers

unpackTx :: SignedBox ByteString (Encryption L4Proto) -> Maybe MyHashRef
unpackTx tx = do
  (_authorKey, bs) <- unboxSignedBox0 tx
  AnnotatedHashRef _ (HashRef messageHashRef) <- eitherToMaybe $ deserialiseOrFail $ BSL.fromStrict bs
  pure $ MyHashRef messageHashRef

syncDBWithRefChan :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m ()
syncDBWithRefChan refChan = do
  allChatMessages <- getAllChatMessagesFromRefChan refChan
  forM_ allChatMessages (processMessage True refChan)

getAllChatMessagesFromRefChan :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m [MyHashRef]
getAllChatMessagesFromRefChan refChan = do
  refChanAPI <- asks refChanAPI
  storageAPI <- asks storageAPI
  refChanHashRef <-
    callService @RpcRefChanGet refChanAPI (fromMyPublicKey refChan)
      >>= orThrow (ServerError "can't request refchan")
      >>= orThrow (ServerError "refchan not found")
  let storage = AnyStorage (StorageClient storageAPI)
  S.toList_ $ walkMerkle (fromHashRef refChanHashRef) (getBlock storage) $ \case
    Left{} -> pure ()
    Right hashRefs -> do
      for_ @[] hashRefs $ \h -> void $ runMaybeT do
        s <-
          getBlock storage (fromHashRef h)
            >>= toMPlus
              <&> deserialiseOrFail @(RefChanUpdate L4Proto)
            >>= toMPlus
        case s of
          Accept{} -> pure ()
          Propose _ box -> do
            (_peerKey, ProposeTran _ tx :: ProposeTran L4Proto) <- toMPlus $ unboxSignedBox0 box
            case unpackTx tx of
              Nothing -> pure ()
              Just messageHashRef -> lift $ S.yield messageHashRef

readRefChanHead :: (MonadUnliftIO m, MonadReader Env m) => HashRef -> m (Maybe (RefChanHeadBlock L4Proto))
readRefChanHead refChanHeadHashRef = do
  storageAPI <- asks storageAPI
  let storage = AnyStorage (StorageClient storageAPI)
  runMaybeT do
    headBlob <- MaybeT $ readBlobFromTree (getBlock storage) refChanHeadHashRef
    (_, headBlock) <- MaybeT $ pure $ unboxSignedBox @_ @'HBS2Basic headBlob
    pure headBlock

getRefChanHeadMembers :: RefChanHeadBlock L4Proto -> RefChanMembers
getRefChanHeadMembers refChanHead =
  RefChanMembers
    { refChanMembersReaders = readers
    , refChanMembersAuthors = authors
    }
 where
  readers = MyEncryptionPublicKey <$> HS.toList (view refChanHeadReaders refChanHead)
  authors = MyPublicKey <$> HS.toList (view refChanHeadAuthors refChanHead)

getRefChanMembers :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m RefChanMembers
getRefChanMembers refChan = do
  storageAPI <- asks storageAPI
  let storage = AnyStorage (StorageClient storageAPI)
  refChanHead <- getRefChanHead @L4Proto storage (RefChanHeadKey $ fromMyPublicKey refChan) >>= orThrow (ServerError "can't request refchan head")
  pure $ getRefChanHeadMembers refChanHead

getWSMembersFromRefChanMembers :: (MonadUnliftIO m, MonadReader Env m) => RefChanMembers -> MyRefChan -> m WSMembers
getWSMembersFromRefChanMembers refChanMembers refChan = do
  authors <- forM (refChanMembersAuthors refChanMembers) \authorKey -> do
    authorName <- withDB $ selectUsername authorKey refChan
    pure $
      AuthorMember
        { authorMemberKey = authorKey
        , authorMemberName = authorName
        }
  let readers = ReaderMember <$> refChanMembersReaders refChanMembers
  pure $
    WSMembers
      { wsMembersAuthors = authors
      , wsMembersReaders = readers
      }

getWSMembersFromRefChan :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m WSMembers
getWSMembersFromRefChan refChan = do
  refChanMembers <- getRefChanMembers refChan
  getWSMembersFromRefChanMembers refChanMembers refChan
