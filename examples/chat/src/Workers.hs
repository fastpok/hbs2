module Workers where

import Control.Monad.Reader
import DB
import DBPipe.SQLite
import Env
import HBS2.Actors.Peer
import HBS2.Peer.Notify
import HBS2.Peer.RPC.Client.Unix
import UnliftIO
import Workers.DownloadQueue
import Workers.RefChan
import Workers.Web

dbWorker :: (MonadReader Env m, MonadUnliftIO m) => m ()
dbWorker = do
  initDBTables
  dbEnv' <- asks dbEnv
  runPipe dbEnv'

runWorkers :: (MonadReader Env m, MonadUnliftIO m) => m [Async ()]
runWorkers = do
  rpcSockPath' <- asks rpcSockPath

  dbWorkerAsync <- async dbWorker
  webWorkerAsync <- async webWorker

  client <- newMessagingUnix False 1.0 rpcSockPath'
  messagingUnixAsync <- async $ runMessagingUnix client
  serviceClientWorkerAsync <- async $ serviceClientWorker client

  refChanNotifyClient <- newMessagingUnix False 1.0 rpcSockPath'
  refChanNotifyMessagingUnixAsync <- async $ runMessagingUnix refChanNotifyClient
  refChanNotifyProtoWorkerAsync <- async $ refChanNotifyProtoWorker refChanNotifyClient
  refChanNotifyClientWorkerAsync <- async $ refChanNotifyClientWorker refChanNotifyClient

  refChanTxNotifyClient <- newMessagingUnix False 1.0 rpcSockPath'
  refChanTxNotifyMessagingUnixAsync <- async $ runMessagingUnix refChanTxNotifyClient
  refChanTxNotifyProtoWorkerAsync <- async $ refChanTxNotifyProtoWorker refChanTxNotifyClient
  refChanTxNotifyClientWorkerAsync <- async $ refChanTxNotifyClientWorker refChanTxNotifyClient

  refChanWorkerAsync <- async refChanWorker

  downloadQueueWorkerAsync <- async downloadQueueWorker

  pure
    [ dbWorkerAsync
    , webWorkerAsync
    , messagingUnixAsync
    , serviceClientWorkerAsync
    , refChanNotifyMessagingUnixAsync
    , refChanNotifyProtoWorkerAsync
    , refChanNotifyClientWorkerAsync
    , refChanTxNotifyMessagingUnixAsync
    , refChanTxNotifyProtoWorkerAsync
    , refChanTxNotifyClientWorkerAsync
    , refChanWorkerAsync
    , downloadQueueWorkerAsync
    ]

serviceClientWorker :: (MonadReader Env m, MonadUnliftIO m) => MessagingUnix -> m ()
serviceClientWorker client = do
  refChanAPI' <- asks refChanAPI
  storageAPI' <- asks storageAPI
  let endpoints =
        [ Endpoint @UNIX refChanAPI'
        , Endpoint @UNIX storageAPI'
        ]
  liftIO $ runReaderT (runServiceClientMulti endpoints) client

refChanNotifyProtoWorker :: (MonadReader Env m, MonadUnliftIO m) => MessagingUnix -> m ()
refChanNotifyProtoWorker client = do
  sink <- asks refChanNotifySink
  liftIO $ flip runReaderT client $ do
    runProto @UNIX
      [ makeResponse (makeNotifyClient sink)
      ]

refChanNotifyClientWorker :: (MonadReader Env m, MonadUnliftIO m) => MessagingUnix -> m ()
refChanNotifyClientWorker client = do
  sink <- asks refChanNotifySink
  liftIO $ flip runReaderT client $ do
    runNotifyWorkerClient sink

refChanTxNotifyProtoWorker :: (MonadReader Env m, MonadUnliftIO m) => MessagingUnix -> m ()
refChanTxNotifyProtoWorker client = do
  sink <- asks refChanTxNotifySink
  liftIO $ flip runReaderT client $ do
    runProto @UNIX
      [ makeResponse (makeNotifyClient sink)
      ]

refChanTxNotifyClientWorker :: (MonadReader Env m, MonadUnliftIO m) => MessagingUnix -> m ()
refChanTxNotifyClientWorker client = do
  sink <- asks refChanTxNotifySink
  liftIO $ flip runReaderT client $ do
    runNotifyWorkerClient sink