module Env
  ( Env (..),
    withEnv,
  )
where

import Config
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.String (IsString (..))
import HBS2.OrDie
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Unix
import UnliftIO.Async

data Env = Env
  { config :: Config,
    refChanAPI :: ServiceCaller RefChanAPI UNIX,
    storageAPI :: ServiceCaller StorageAPI UNIX
  }

withEnv :: (MonadUnliftIO m) => Config -> (Env -> m a) -> m a
withEnv config action = do
  rpcSockPath <- detectRPC `orDie` "can't locate hbs2-peer rpc"
  client <- newMessagingUnix False 1.0 rpcSockPath
  void $ async $ runMessagingUnix client
  let peer = fromString rpcSockPath
  refChanAPI <- makeServiceCaller @RefChanAPI peer
  storageAPI <- makeServiceCaller @StorageAPI peer
  let endpoints =
        [ Endpoint @UNIX refChanAPI,
          Endpoint @UNIX storageAPI
        ]
  void $ async $ liftIO $ runReaderT (runServiceClientMulti endpoints) client
  let env =
        Env
          { config = config,
            refChanAPI = refChanAPI,
            storageAPI = storageAPI
          }
  action env
