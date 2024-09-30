module Workers.Web (webWorker) where

import Codec.Serialise
import Control.Monad.Cont
import Control.Monad.Reader
import DB
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time
import Data.UUID.V4 qualified as UUID
import Env
import Error
import HBS2.Data.Types.SignedBox
import HBS2.KeyMan.Keys.Direct
import HBS2.Net.Auth.Credentials hiding (encode)
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.OrDie
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.Unix hiding (encode)
import HBS2.Prelude (headMay)
import Monad
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWS
import Network.Wai.Middleware.Static
import Network.WebSockets qualified as WS
import Pages.Login
import Pages.Main
import Types
import UnliftIO
import Web.Scotty.Trans

webWorker :: (MonadReader Env m, MonadUnliftIO m) => m ()
webWorker = do
  let port = 3000
  let settings = Warp.setPort port Warp.defaultSettings
  scottyApp <- makeScottyApp
  env <- ask
  liftIO $ putStrLn $ "Running server on port " <> show port
  liftIO $ Warp.runSettings settings $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp env) scottyApp

makeScottyApp :: (MonadReader Env m, MonadUnliftIO m) => m Wai.Application
makeScottyApp = do
  env <- ask
  scottyAppT (runIO env) myScottyApp
  where
    runIO :: Env -> AppM a -> IO a
    runIO env m = runReaderT (runAppM m) env

myScottyApp :: ScottyT AppM ()
myScottyApp = do
  middleware $ staticPolicy (noDots >-> addBase "static")
  defaultHandler exceptionHandler
  get "/" mainPage
  get "/login" loginPage

wsApp :: Env -> WS.ServerApp
wsApp env pending = do
  conn <- WS.acceptRequest pending
  WS.withPingPong
    WS.defaultPingPongOptions
    conn
    \connection -> runReaderT (runAppM (myWSApp connection)) env

wsMessageToMessage :: (MonadUnliftIO m) => WSMessage -> m Message
wsMessageToMessage wsMessage = do
  currentTime <- liftIO getCurrentTime
  pure $
    Message
      { messageAuthor = MyPublicKey $ sigilSignPk $ fromMySigil $ wsMessageAuthor wsMessage,
        messageChat = wsMessageChat wsMessage,
        messageBody = wsMessageBody wsMessage,
        messageCreatedAt = currentTime
      }

myWSApp :: WS.Connection -> AppM ()
myWSApp conn = do
  let wsSession =
        WSSession
          { wsSessionConn = conn,
            wsSessionSubscriptions = []
          }
  wsSessionID <- addWSSession wsSession
  let disconnect = removeWSSession wsSessionID
  flip finally disconnect $ do
    receiveLoop' <- async (receiveLoop conn wsSessionID)
    sendLoop' <- async (sendLoop conn wsSessionID)
    void $ waitAnyCancel [receiveLoop', sendLoop']

receiveLoop :: (MonadReader Env m, MonadUnliftIO m) => WS.Connection -> WSSessionID -> m ()
receiveLoop conn sessionID = do
  forever $ do
    wsData <- liftIO $ WS.receiveData conn
    case wsData of
      WSProtocolSubscribe wsSubscribe -> do
        addSubscription sessionID (wsSubscribeRefChan wsSubscribe)
      WSProtocolMessage wsMessage -> do
        message <- wsMessageToMessage wsMessage
        withDB $ insertMessage message
        postMessageToRefChan message
      _ -> liftIO $ WS.sendTextData conn WSErrorBadMessage

sendLoop :: (MonadReader Env m, MonadUnliftIO m) => WS.Connection -> WSSessionID -> m ()
sendLoop conn sessionID = do
  wsSessionsTVar <- asks wsSessionsTVar
  chatUpdatesChan' <- asks chatUpdatesChan
  myChatUpdatesChan <- atomically $ dupTChan chatUpdatesChan'
  forever $ do
    chatUpdate <- atomically $ readTChan myChatUpdatesChan
    wsSessionsTVar' <- readTVarIO wsSessionsTVar
    let session = wsSessionsTVar' Map.! sessionID
    case headMay $ wsSessionSubscriptions session of
      Nothing -> pure ()
      Just chat -> do
        when (chatUpdate == chat) $ do
          messages <- withDB $ selectChatMessages chat
          liftIO $ WS.sendTextData conn $ WSProtocolMessages $ WSMessages messages

postMessageToRefChan :: (MonadReader Env m, MonadUnliftIO m) => Message -> m ()
postMessageToRefChan message = do
  creds <- liftIO $ runKeymanClient do
    loadCredentials (messageAuthor message) >>= orThrow (RequestError "can't load credentials")
  -- creds contains message author keys, not peer keys, right?
  let box = makeSignedBox (_peerSignPk creds) (_peerSignSk creds) (BSL.toStrict $ serialise message)
  refChanAPI <- asks refChanAPI
  void $ callService @RpcRefChanPropose refChanAPI (fromMyPublicKey $ messageChat message, box)

addWSSession :: (MonadReader Env m, MonadUnliftIO m) => WSSession -> m WSSessionID
addWSSession wsSession = do
  wsSessionsTVar <- asks wsSessionsTVar
  wsSessionID <- liftIO UUID.nextRandom
  atomically $ modifyTVar wsSessionsTVar (Map.insert wsSessionID wsSession)
  pure wsSessionID

-- wsSessionExists :: (MonadReader Env m, MonadUnliftIO m) => WSSessionID -> m Bool
-- wsSessionExists wsSessionID = do
--   wsSessionsTVar <- asks wsSessionsTVar
--   wsSessionsTVar' <- readTVarIO wsSessionsTVar
--   pure $ Map.member wsSessionID wsSessionsTVar'

removeWSSession :: (MonadReader Env m, MonadUnliftIO m) => WSSessionID -> m ()
removeWSSession wsSessionID = do
  wsSessionsTVar <- asks wsSessionsTVar
  atomically $ modifyTVar wsSessionsTVar (Map.delete wsSessionID)

addSubscription :: (MonadReader Env m, MonadUnliftIO m) => WSSessionID -> MyRefChan -> m ()
addSubscription wsSessionID refChan = do
  wsSessionsTVar <- asks wsSessionsTVar
  atomically $ modifyTVar wsSessionsTVar (Map.adjust addSub wsSessionID)
  where
    addSub session =
      session
        { wsSessionSubscriptions = [refChan]
        -- for now we only have 1 active sub
        -- { wsSessionSubscriptions = refChan : wsSessionSubscriptions session
        }
