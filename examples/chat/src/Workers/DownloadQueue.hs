module Workers.DownloadQueue where

import Control.Monad
import Control.Monad.Reader
import Env
import HBS2.Clock
import Message
import Types
import UnliftIO

downloadQueueWorker :: (MonadReader Env m, MonadUnliftIO m) => m ()
downloadQueueWorker = do
  messageDownloadQueue' <- asks messageDownloadQueue
  chatEventsChan' <- asks chatEventsChan
  forever do
    messageDownloadQueueItem@MessageDownloadQueueItem{..} <- atomically $ readTQueue messageDownloadQueue'
    processMessageResult <- processMessage False messageDownloadQueueItemRefChan messageDownloadQueueItemHashRef
    case processMessageResult of
      Nothing -> do
        -- TODO: exponential backoff?
        -- TODO: this worker algorithm doesn't seem optimal
        pause @'Seconds 0.1
        atomically $ writeTQueue messageDownloadQueue' messageDownloadQueueItem
      Just decryptedMessage -> do
        atomically $
          writeTChan chatEventsChan' $
            MessageDownloadedEvent
              { messageDownloadedEventRefChan = messageDownloadQueueItemRefChan
              , messageDownloadedEventMessage = decryptedMessage
              }
        pure ()
