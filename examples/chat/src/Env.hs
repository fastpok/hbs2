module Env
  ( Env (..),
    withEnv,
  )
where

import Config
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.String (IsString (..))
import HBS2.Clock
import HBS2.OrDie
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.Unix
import Prettyprinter
import UnliftIO.Async

data Env = Env
  { config :: Config,
    refChanAPI :: ServiceCaller RefChanAPI UNIX
  }

withEnv :: (MonadUnliftIO m) => Config -> (Env -> m a) -> m a
withEnv config action = do
  rpcSockPath <- detectRPC `orDie` "can't locate hbs2-peer rpc"
  client <-
    liftIO $
      race (pause @'Seconds 1) (newMessagingUnix False 1.0 rpcSockPath)
        >>= orThrowUser
          ("can't connect to" <+> pretty rpcSockPath)
  void $ liftIO $ async $ runMessagingUnix client
  refChanAPI <- makeServiceCaller @RefChanAPI (fromString rpcSockPath)
  let endpoints = [Endpoint @UNIX refChanAPI]
  void $ async $ liftIO $ runReaderT (runServiceClientMulti endpoints) client
  let env =
        Env
          { config = config,
            refChanAPI = refChanAPI
          }
  action env