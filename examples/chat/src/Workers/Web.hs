module Workers.Web (webWorker) where

import Codec.Serialise
import Control.Monad.Cont
import Control.Monad.Reader
import DB
import Data.ByteString.Lazy qualified as BSL
import Data.Time
import Env
import Error
import HBS2.Clock
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

messageReqToMessage :: (MonadUnliftIO m) => MessageReq -> m Message
messageReqToMessage messageReq = do
  currentTime <- liftIO getCurrentTime
  pure $
    Message
      { messageAuthor = MyPublicKey $ sigilSignPk $ messageReqAuthor messageReq,
        messageChat = messageReqChat messageReq,
        messageBody = messageReqBody messageReq,
        messageCreatedAt = currentTime
      }

myWSApp :: WS.Connection -> AppM ()
myWSApp conn = do
  receiveLoop' <- async (receiveLoop conn)
  sendLoop' <- async (sendLoop conn)
  void $ waitAnyCatchCancel [receiveLoop', sendLoop']

receiveLoop :: (MonadReader Env m, MonadUnliftIO m) => WS.Connection -> m ()
receiveLoop conn = forever $ do
  messageReq <- liftIO $ WS.receiveData conn
  message <- messageReqToMessage messageReq
  withDB $ insertMessage message
  postMessageToRefChan message

sendLoop :: (MonadReader Env m, MonadUnliftIO m) => WS.Connection -> m ()
sendLoop conn = forever $ do
  pause @'Seconds 2
  messages <- withDB selectMessages
  liftIO $ WS.sendTextData conn $ Messages messages

postMessageToRefChan :: (MonadReader Env m, MonadUnliftIO m) => Message -> m ()
postMessageToRefChan message = do
  creds <- liftIO $ runKeymanClient do
    loadCredentials (messageAuthor message) >>= orThrow (RequestError "can't load credentials")
  -- these are not peer keys, but message author keys, right?
  let box = makeSignedBox (_peerSignPk creds) (_peerSignSk creds) (BSL.toStrict $ serialise message)
  refChanAPI <- asks refChanAPI
  void $ callService @RpcRefChanPropose refChanAPI (fromMyPublicKey $ messageChat message, box)
