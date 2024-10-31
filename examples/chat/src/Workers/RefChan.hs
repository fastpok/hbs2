module Workers.RefChan where

import Codec.Serialise
import Config
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import DB
import Data.ByteString.Lazy qualified as BSL
import Data.HashSet qualified as HS
import Env
import Error
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Data.Types.SignedBox
import HBS2.KeyMan.Keys.Direct
import HBS2.Merkle
import HBS2.Net.Auth.Credentials hiding (encode)
import HBS2.Net.Proto.Notify
import HBS2.OrDie
import HBS2.Peer.Notify
import HBS2.Peer.Proto.Mailbox
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
      sink <- asks refChanNotifySink
      notifyWorkers <- forM someRefChans \refChan -> async do
        let refChanKey = namedRefChanKey refChan
        runNotifySink sink (RefChanNotifyKey $ fromMyPublicKey refChanKey) $ \case
          RefChanUpdated _ _ -> do
            syncDBWithRefChan refChanKey
            atomically $ writeTChan chatEventsChan' $ MessagesEvent refChanKey
          RefChanHeadUpdated _ _ newRefChanHeadHashRef -> do
            refChanHead <- readRefChanHead newRefChanHeadHashRef >>= orThrow (ServerError "can't request refchan head")
            let readers = HS.toList $ view refChanHeadReaders refChanHead
                authors = HS.toList $ view refChanHeadAuthors refChanHead
            atomically $
              writeTChan chatEventsChan' $
                MembersEvent
                  { membersEventRefChan = refChanKey
                  , membersEventAuthors = AuthorMember . MyPublicKey <$> authors
                  , membersEventReaders = ReaderMember . MyEncryptionPublicKey <$> readers
                  }
            pure ()
          _ -> pure ()
      void $ waitAnyCancel notifyWorkers

syncDBWithRefChan :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m ()
syncDBWithRefChan refChan = do
  allChatMessages <- getAllChatMessagesFromRefChan refChan
  let readMessageServices = ReadMessageServices (liftIO . runKeymanClientRO . extractGroupKeySecret)
  forM_ allChatMessages $ \(hashRef, encryptedMessage) -> do
    (authorPublicKey, messageContent, _messageDataBS) <- readMessage readMessageServices encryptedMessage
    let messageMetadata =
          MessageMetadata
            { messageMetaHashRef = hashRef
            , messageMetaChat = refChan
            , messageMetaAuthor = MyPublicKey authorPublicKey
            , messageMetaCreatedAt = getUTCTimeFromMessageTimestamp $ messageCreated $ messageFlags messageContent
            }
    withDB $ insertMessageMetadata messageMetadata

getAllChatMessagesFromRefChan :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m [(MyHashRef, EncryptedMessage)]
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
            -- is this really peer's key ?
            (_peerKey, ProposeTran _ pbox :: ProposeTran L4Proto) <- toMPlus $ unboxSignedBox0 box
            -- is this really author's key ?
            (_authorKey, bs) <- toMPlus $ unboxSignedBox0 pbox
            case deserialiseOrFail $ BSL.fromStrict bs of
              Left _ -> pure ()
              Right (AnnotatedHashRef _ (HashRef msgHashRef)) -> do
                encryptedMessage <- liftIO $ getMessageWait storage (MyHashRef msgHashRef)
                lift $ S.yield (MyHashRef msgHashRef, encryptedMessage)

readRefChanHead :: (MonadUnliftIO m, MonadReader Env m) => HashRef -> m (Maybe (RefChanHeadBlock L4Proto))
readRefChanHead refChanHeadHashRef = do
  storageAPI <- asks storageAPI
  let storage = AnyStorage (StorageClient storageAPI)
  runMaybeT do
    headBlob <- MaybeT $ readBlobFromTree (getBlock storage) refChanHeadHashRef
    (_, headBlock) <- MaybeT $ pure $ unboxSignedBox @_ @'HBS2Basic headBlob
    pure headBlock

getRefChanMembers :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m RefChanMembers
getRefChanMembers refChan = do
  storageAPI <- asks storageAPI
  let storage = AnyStorage (StorageClient storageAPI)
  refChanHead <- getRefChanHead @L4Proto storage (RefChanHeadKey $ fromMyPublicKey refChan) >>= orThrow (ServerError "can't request refchan head")
  let readers = MyEncryptionPublicKey <$> HS.toList (view refChanHeadReaders refChanHead)
      authors = MyPublicKey <$> HS.toList (view refChanHeadAuthors refChanHead)
  pure $
    RefChanMembers
      { refChanMembersReaders = readers
      , refChanMembersAuthors = authors
      }

getChatMembersFromRefChan :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m WSMembers
getChatMembersFromRefChan refChan = do
  refChanMembers <- getRefChanMembers refChan
  pure $
    WSMembers
      { wsMembersAuthors = AuthorMember <$> refChanMembersAuthors refChanMembers
      , wsMembersReaders = ReaderMember <$> refChanMembersReaders refChanMembers
      }
