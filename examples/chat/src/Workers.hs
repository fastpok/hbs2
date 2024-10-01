module Workers where

import Control.Monad.Reader
import DB
import DBPipe.SQLite
import Env
import HBS2.Actors.Peer
import HBS2.Peer.Notify
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
  serviceClientWorker' <- async $ serviceClientWorker client
  refChanNotifyClient <- newMessagingUnix False 1.0 rpcSockPath'
  refChanNotifyMessagingUnix <- async $ runMessagingUnix refChanNotifyClient
  refChanNotifyClientWorker' <- async $ refChanNotifyClientWorker refChanNotifyClient
  refChanNotifyWorker' <- async $ refChanNotifyWorker refChanNotifyClient
  refChanWorker' <- async refChanWorker
  pure
    [ dbWorker',
      webWorker',
      messagingUnix,
      refChanNotifyMessagingUnix,
      serviceClientWorker',
      refChanNotifyClientWorker',
      refChanNotifyWorker',
      refChanWorker'
    ]

serviceClientWorker :: (MonadReader Env m, MonadUnliftIO m) => MessagingUnix -> m ()
serviceClientWorker client = do
  refChanAPI' <- asks refChanAPI
  storageAPI' <- asks storageAPI
  let endpoints =
        [ Endpoint @UNIX refChanAPI',
          Endpoint @UNIX storageAPI'
        ]
  liftIO $ runReaderT (runServiceClientMulti endpoints) client

refChanNotifyClientWorker :: (MonadReader Env m, MonadUnliftIO m) => MessagingUnix -> m ()
refChanNotifyClientWorker client = do
  sink <- asks refChanNotifySink
  liftIO $ flip runReaderT client $ do
    runProto @UNIX
      [ makeResponse (makeNotifyClient sink)
      ]

refChanNotifyWorker :: (MonadReader Env m, MonadUnliftIO m) => MessagingUnix -> m ()
refChanNotifyWorker client = do
  sink <- asks refChanNotifySink
  liftIO $ flip runReaderT client $ do
    runNotifyWorkerClient sink
