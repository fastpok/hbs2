module Workers.Web (webWorker) where

import Codec.Serialise
import Control.Monad
import Control.Monad.Reader
import DB
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Time
import Data.UUID qualified as UUID
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
import Workers.RefChan

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

wsMessageToMessage :: (MonadReader Env m, MonadUnliftIO m) => WSSessionID -> WSMessage -> m Message
wsMessageToMessage wsSessionID (WSMessage wsMessage) = do
  wsSessionsTVar' <- asks wsSessionsTVar
  wsSessions <- readTVarIO wsSessionsTVar'
  wsSession <- orThrow (ServerError $ "session does not exist: " <> UUID.toText wsSessionID) (Map.lookup wsSessionID wsSessions)
  currentTime <- liftIO getCurrentTime
  activeChat <- orThrow (RequestError "active chat is not set") (wsSessionActiveChat wsSession)
  pure $
    Message
      { messageAuthor = MyPublicKey $ sigilSignPk $ fromMySigil $ wsSessionClientSigil wsSession
      , messageChat = activeChat
      , messageBody = wsMessage
      , messageCreatedAt = currentTime
      }

myWSApp :: WS.Connection -> AppM ()
myWSApp conn = do
  wsData <- liftIO $ WS.receiveData conn
  case wsData of
    WSProtocolClientMessageHello wsHello -> do
      let wsSession =
            WSSession
              { wsSessionConn = conn
              , wsSessionActiveChat = Nothing
              , wsSessionClientSigil = wsHelloClientSigil wsHello
              }
      wsSessionID <- addWSSession wsSession
      let disconnect = removeWSSession wsSessionID
      flip finally disconnect $ do
        receiveLoop' <- async (receiveLoop conn wsSessionID)
        sendLoop' <- async (sendLoop conn wsSessionID)
        void $ waitAnyCancel [receiveLoop', sendLoop']
    _ -> do
      liftIO $ WS.sendTextData conn WSErrorBadHello
      myWSApp conn

receiveLoop :: (MonadReader Env m, MonadUnliftIO m) => WS.Connection -> WSSessionID -> m ()
receiveLoop conn sessionID = do
  forever $ do
    wsData <- liftIO $ WS.receiveData conn
    case wsData of
      WSProtocolClientMessageActiveChat activeChat -> do
        let chat = fromWSActiveChat activeChat
        setActiveChat sessionID chat
        loadChatMessages chat
        messages <- withDB $ selectChatMessages pageSize Nothing chat
        liftIO $ WS.sendTextData conn $ WSProtocolServerMessageMessages $ WSMessages messages
        members <- getChatMembersFromRefChan chat
        liftIO $ WS.sendTextData conn $ WSProtocolServerMessageMembers members
      WSProtocolClientMessageMessage wsMessage -> do
        message <- wsMessageToMessage sessionID wsMessage
        withDB $ insertMessage message
        postMessageToRefChan message
      WSProtocolClientMessageGetMessages WSGetMessages{..} -> do
        wsSessionsTVar' <- asks wsSessionsTVar
        wsSessions <- readTVarIO wsSessionsTVar'
        wsSession <- orThrow (ServerError $ "session does not exist: " <> UUID.toText sessionID) (Map.lookup sessionID wsSessions)
        activeChat <- orThrow (RequestError "active chat is not set") (wsSessionActiveChat wsSession)
        messages <- withDB $ selectChatMessages wsGetMessagesLimit (Just wsGetMessagesCursor) activeChat
        liftIO $ WS.sendTextData conn $ WSProtocolServerMessageMessages $ WSMessages messages
      WSProtocolClientMessageHello _ -> liftIO $ WS.sendTextData conn WSErrorDuplicateHello

sendLoop :: (MonadReader Env m, MonadUnliftIO m) => WS.Connection -> WSSessionID -> m ()
sendLoop conn sessionID = do
  wsSessionsTVar' <- asks wsSessionsTVar
  chatEventsChan' <- asks chatEventsChan
  myChatEventsChan <- atomically $ dupTChan chatEventsChan'
  forever $ do
    chatEvent <- atomically $ readTChan myChatEventsChan
    wsSessions <- readTVarIO wsSessionsTVar'
    session <- orThrow (ServerError $ "session does not exist: " <> UUID.toText sessionID) (Map.lookup sessionID wsSessions)
    case wsSessionActiveChat session of
      Nothing -> pure ()
      Just activeChat -> case chatEvent of
        MessagesEvent eventChat -> when (eventChat == activeChat) $ do
          messages <- withDB $ selectChatMessages pageSize Nothing activeChat
          liftIO $ WS.sendTextData conn $ WSProtocolServerMessageMessages $ WSMessages messages
        MembersEvent{..} -> when (membersEventRefChan == activeChat) $ do
          liftIO $
            WS.sendTextData conn $
              WSProtocolServerMessageMembers $
                WSMembers
                  { wsMembersReaders = membersEventReaders
                  , wsMembersAuthors = membersEventAuthors
                  }

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

setActiveChat :: (MonadReader Env m, MonadUnliftIO m) => WSSessionID -> MyRefChan -> m ()
setActiveChat wsSessionID chat = do
  wsSessionsTVar' <- asks wsSessionsTVar
  atomically $
    modifyTVar wsSessionsTVar' $
      Map.adjust
        ( \session ->
            session
              { wsSessionActiveChat = Just chat
              }
        )
        wsSessionID
