module Workers where

import Control.Monad.Reader
import DB
import DBPipe.SQLite
import Env
import HBS2.Peer.RPC.Client.Unix
import UnliftIO
import Workers.RefChan
import Workers.Web

dbWorker :: (MonadReader Env m, MonadUnliftIO m) => m ()
dbWorker = do
  initDBTables
  dbEnv' <- asks dbEnv
  runPipe dbEnv'

runWorkers :: (MonadReader Env m, MonadUnliftIO m) => m [Async ()]
runWorkers = do
  dbWorker' <- async dbWorker
  webWorker' <- async $ webWorker
  rpcSockPath' <- asks rpcSockPath
  client <- newMessagingUnix False 1.0 rpcSockPath'
  messagingUnix <- async $ runMessagingUnix client
  refChanAPI' <- asks refChanAPI
  storageAPI' <- asks storageAPI
  let endpoints =
        [ Endpoint @UNIX refChanAPI',
          Endpoint @UNIX storageAPI'
        ]
  serviceClient <- async $ liftIO $ runReaderT (runServiceClientMulti endpoints) client
  refChanWorker' <- async $ refChanWorker
  pure
    [ webWorker',
      messagingUnix,
      serviceClient,
      dbWorker',
      refChanWorker'
    ]