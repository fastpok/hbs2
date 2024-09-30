module Env
  ( Env (..),
    initEnv,
  )
where

import Config
import Control.Monad.IO.Unlift
import DBPipe.SQLite
import Data.String (IsString (..))
import HBS2.OrDie
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Unix
import System.Directory
import System.FilePath
import Types
import UnliftIO

data Env = Env
  { config :: Config,
    refChanAPI :: ServiceCaller RefChanAPI UNIX,
    storageAPI :: ServiceCaller StorageAPI UNIX,
    rpcSockPath :: FilePath,
    dbEnv :: DBPipeEnv,
    wsClients :: TVar [WSClient]
  }

initEnv :: (MonadUnliftIO m) => Config -> m Env
initEnv config = do
  rpcSockPath <- detectRPC `orDie` "can't locate hbs2-peer rpc"
  let peer = fromString rpcSockPath
  refChanAPI <- makeServiceCaller @RefChanAPI peer
  storageAPI <- makeServiceCaller @StorageAPI peer
  dbEnv <- initDBEnv $ dbPath config
  wsClients <- newTVarIO []
  pure $ Env {..}

initDBEnv :: (MonadUnliftIO m) => Maybe FilePath -> m DBPipeEnv
initDBEnv maybeDBPath = do
  dbPath <-
    case maybeDBPath of
      Nothing -> do
        defDBDir <- liftIO $ getXdgDirectory XdgData appName
        liftIO $ createDirectoryIfMissing True defDBDir
        pure $ defDBDir </> "state.db"
      Just dbPath -> do
        liftIO $ createDirectoryIfMissing True $ takeDirectory dbPath
        pure $ dbPath
  newDBPipeEnv dbPipeOptsDef dbPath
