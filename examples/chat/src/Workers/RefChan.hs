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
import Streaming.Prelude qualified as S
import Types
import UnliftIO

refChanWorker :: (MonadUnliftIO m, MonadReader Env m) => m ()
refChanWorker = do
  refChans' <- asks (refChans . config)
  chatEventsChan' <- asks chatEventsChan
  sink <- asks refChanNotifySink
  notifyWorkers <- forM refChans' \refChan -> async do
    runNotifySink sink (RefChanNotifyKey $ fromMyPublicKey refChan) $ \case
      RefChanUpdated _ _ -> do
        loadChatMessages refChan
        atomically $ writeTChan chatEventsChan' $ MessagesEvent refChan
      RefChanHeadUpdated _ _ newRefChanHeadHashRef -> do
        refChanHead <- readRefChanHead newRefChanHeadHashRef >>= orThrow (ServerError "can't request refchan head")
        let readers = HS.toList $ view refChanHeadReaders refChanHead
            authors = HS.toList $ view refChanHeadAuthors refChanHead
        atomically $
          writeTChan chatEventsChan' $
            MembersEvent
              { membersEventRefChan = refChan,
                membersEventAuthors = AuthorMember . MyPublicKey <$> authors,
                membersEventReaders = ReaderMember . MyEncryptionPublicKey <$> readers
              }
        pure ()
      _ -> pure ()
  void $ waitAnyCancel notifyWorkers

loadChatMessages :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m ()
loadChatMessages refChan = do
  allChatMessages <- getAllChatMessages refChan
  forM_ allChatMessages (withDB . insertMessage)

getAllChatMessages :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m [Message]
getAllChatMessages refChan = do
  refChanAPI <- asks refChanAPI
  storageAPI <- asks storageAPI
  hashRef <-
    callService @RpcRefChanGet refChanAPI (fromMyPublicKey refChan)
      >>= orThrow (ServerError "can't request refchan")
      >>= orThrow (ServerError "refchan not found")
  let storage = AnyStorage (StorageClient storageAPI)

  S.toList_ $ walkMerkle (fromHashRef hashRef) (getBlock storage) $ \case
    Left {} -> pure ()
    Right hashRefs -> do
      for_ @[] hashRefs $ \h -> void $ runMaybeT do
        s <-
          getBlock storage (fromHashRef h)
            >>= toMPlus
              <&> deserialiseOrFail @(RefChanUpdate L4Proto)
            >>= toMPlus
        case s of
          Accept {} -> pure ()
          Propose _ box -> do
            -- is this really peer's key ?
            (_peerKey, ProposeTran _ pbox :: ProposeTran L4Proto) <- toMPlus $ unboxSignedBox0 box
            -- is this really author's key ?
            (_authorKey, bs) <- toMPlus $ unboxSignedBox0 pbox
            case deserialiseOrFail $ BSL.fromStrict bs of
              Left _ -> do
                pure ()
              Right msg ->
                lift $
                  S.yield msg

readRefChanHead :: (MonadUnliftIO m, MonadReader Env m) => HashRef -> m (Maybe (RefChanHeadBlock L4Proto))
readRefChanHead refChanHeadHashRef = do
  storageAPI <- asks storageAPI
  let storage = AnyStorage (StorageClient storageAPI)
  runMaybeT do
    headBlob <- MaybeT $ readBlobFromTree (getBlock storage) refChanHeadHashRef
    (_, headBlock) <- MaybeT $ pure $ unboxSignedBox @_ @'HBS2Basic headBlob
    pure headBlock

getChatMembersFromRefChan :: (MonadUnliftIO m, MonadReader Env m) => MyRefChan -> m WSMembers
getChatMembersFromRefChan refChan = do
  storageAPI <- asks storageAPI
  let storage = AnyStorage (StorageClient storageAPI)
  refChanHead <- getRefChanHead @L4Proto storage (RefChanHeadKey $ fromMyPublicKey refChan) >>= orThrow (ServerError "can't request refchan head")
  let readers = HS.toList $ view refChanHeadReaders refChanHead
      authors = HS.toList $ view refChanHeadAuthors refChanHead
  pure $
    WSMembers
      { wsMembersAuthors = AuthorMember . MyPublicKey <$> authors,
        wsMembersReaders = ReaderMember . MyEncryptionPublicKey <$> readers
      }