module Workers.RefChan where

import Codec.Serialise
import Config
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import DB
import Data.ByteString.Lazy qualified as BSL
import Env
import Error
import HBS2.Clock
import HBS2.Data.Types
import HBS2.Data.Types.SignedBox
import HBS2.Merkle
import HBS2.Net.Auth.Credentials hiding (encode)
import HBS2.OrDie
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.RPC.Client.Unix hiding (encode)
import HBS2.Prelude hiding (line)
import HBS2.Storage
import Streaming.Prelude qualified as S
import Types
import UnliftIO

refChanWorker :: (MonadUnliftIO m, MonadReader Env m) => m ()
refChanWorker = do
  config' <- asks config
  let refChans' = refChans config'
  fix \next -> do
    forM_ refChans' loadChatMessages
    pause @Seconds 2
    next

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