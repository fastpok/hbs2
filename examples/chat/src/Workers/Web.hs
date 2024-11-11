module Workers.Web (webWorker) where

import Codec.Serialise
import Control.Monad
import Control.Monad.Reader
import DB
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Env
import Error
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.KeyMan.Keys.Direct
import HBS2.Net.Auth.Credentials hiding (encode)
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.OrDie
import HBS2.Peer.Proto.Mailbox.Message hiding (createMessage)
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.RPC.Client.Unix hiding (encode)
import HBS2.Storage
import Message
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
  scottyAppT (runIO env) (myScottyApp (staticPath env))
 where
  runIO :: Env -> AppM a -> IO a
  runIO env m = runReaderT (runAppM m) env

myScottyApp :: FilePath -> ScottyT AppM ()
myScottyApp staticPath = do
  middleware $ staticPolicy (noDots >-> addBase staticPath)
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

createEncryptedMessage :: (MonadReader Env m, MonadUnliftIO m) => WSMessage -> WSSessionID -> m EncryptedMessage
createEncryptedMessage (WSMessage wsMessage) sessionID = do
  wsSessionsTVar' <- asks wsSessionsTVar
  wsSessions <- readTVarIO wsSessionsTVar'
  wsSession <- orThrow (ServerError $ "session does not exist: " <> UUID.toText sessionID) (Map.lookup sessionID wsSessions)
  messageFlags <- defMessageFlags
  storageAPI <- asks storageAPI
  activeChat <- orThrow (RequestError "active chat is not set") (wsSessionActiveChat wsSession)
  refChanMembers <- getRefChanMembers activeChat
  let storage = AnyStorage (StorageClient storageAPI)
      createMessageServices =
        CreateMessageServices
          storage
          (runKeymanClientRO . loadCredentials)
          (runKeymanClientRO . loadKeyRingEntry)
      sender = Right $ fromMySigil $ wsSessionClientSigil wsSession
      recipients = fromMyEncryptionPublicKey <$> refChanMembersReaders refChanMembers
  createMessage createMessageServices messageFlags Nothing sender recipients mempty (TE.encodeUtf8 wsMessage)

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
              , wsSessionMessages = Set.empty
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

getDecryptedMessageByMetadata :: (MonadReader Env m, MonadUnliftIO m) => MessageMetadata -> m DecryptedMessage
getDecryptedMessageByMetadata MessageMetadata{..} = do
  storageAPI <- asks storageAPI
  let storage = AnyStorage (StorageClient storageAPI)
      readMessageServices = ReadMessageServices (liftIO . runKeymanClientRO . extractGroupKeySecret)
  encryptedMessage <- getMessageWait storage messageMetaHashRef
  (_authorPublicKey, _messageContent, messageDataBS) <- readMessage readMessageServices encryptedMessage
  maybeUsername <- withDB $ selectUsername messageMetaAuthor messageMetaChat
  pure $
    DecryptedMessage
      { decryptedMessageHashRef = messageMetaHashRef
      , decryptedMessageAuthorKey = messageMetaAuthor
      , decryptedMessageAuthorName = maybeUsername
      , decryptedMessageChat = messageMetaChat
      , decryptedMessageCreatedAt = messageMetaCreatedAt
      , decryptedMessageBody = TE.decodeUtf8 messageDataBS
      }

receiveLoop :: (MonadReader Env m, MonadUnliftIO m) => WS.Connection -> WSSessionID -> m ()
receiveLoop conn sessionID = do
  forever $ do
    wsData <- liftIO $ WS.receiveData conn
    case wsData of
      WSProtocolClientMessageActiveChat activeChat -> do
        let chat = fromWSActiveChat activeChat
        setActiveChat sessionID chat
        syncDBWithRefChan chat
        messageMeta <- withDB $ selectChatMessageMetadata pageSize Nothing chat
        messages <- mapM getDecryptedMessageByMetadata messageMeta
        liftIO $
          WS.sendTextData conn $
            WSProtocolServerMessageOldMessages $
              WSOldMessages
                { wsOldMessagesHXSwap = WSOldMessagesHXSwapInnerHTML
                , wsOldMessages = messages
                }
        addSessionMessageHashRefs sessionID $ Set.fromList $ decryptedMessageHashRef <$> messages
        members <- getWSMembersFromRefChan chat
        liftIO $ WS.sendTextData conn $ WSProtocolServerMessageMembers members
      WSProtocolClientMessageMessage wsMessage -> do
        encryptedMessage <- createEncryptedMessage wsMessage sessionID
        storageAPI <- asks storageAPI
        let storage = AnyStorage (StorageClient storageAPI)
        encryptedMessageHashRef <-
          putBlock storage (serialise encryptedMessage)
            >>= orThrow (ServerError "can't store message") . fmap MyHashRef
        wsSessionsTVar' <- asks wsSessionsTVar
        wsSessions <- readTVarIO wsSessionsTVar'
        wsSession <- orThrow (ServerError $ "session does not exist: " <> UUID.toText sessionID) (Map.lookup sessionID wsSessions)
        activeChat <- orThrow (RequestError "active chat is not set") (wsSessionActiveChat wsSession)
        let author = MyPublicKey $ sigilSignPk $ fromMySigil $ wsSessionClientSigil wsSession
        postHashRefToRefChan author activeChat encryptedMessageHashRef
      WSProtocolClientMessageGetMessages WSGetMessages{..} -> do
        wsSessionsTVar' <- asks wsSessionsTVar
        wsSessions <- readTVarIO wsSessionsTVar'
        wsSession <- orThrow (ServerError $ "session does not exist: " <> UUID.toText sessionID) (Map.lookup sessionID wsSessions)
        activeChat <- orThrow (RequestError "active chat is not set") (wsSessionActiveChat wsSession)
        messageMeta <- withDB $ selectChatMessageMetadata wsGetMessagesLimit (Just $ BeforeCursor wsGetMessagesCursor) activeChat
        messages <- mapM getDecryptedMessageByMetadata messageMeta
        liftIO $
          WS.sendTextData conn $
            WSProtocolServerMessageOldMessages $
              WSOldMessages
                { wsOldMessagesHXSwap = WSOldMessagesHXSwapBeforeEnd
                , wsOldMessages = messages
                }
        addSessionMessageHashRefs sessionID $ Set.fromList $ decryptedMessageHashRef <$> messages
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
          -- TODO: it would be nice not to get all the transactions from DB in the search for new ones
          messageMeta <- withDB $ selectChatMessageMetadata pageSize Nothing activeChat
          let newMessageMeta = filter (\meta -> Set.notMember (messageMetaHashRef meta) (wsSessionMessages session)) messageMeta
          forM_ newMessageMeta \meta -> do
            newMessage <- getDecryptedMessageByMetadata meta
            let sessionClientPublicKey = MyPublicKey $ sigilSignPk $ fromMySigil $ wsSessionClientSigil session
            liftIO $
              WS.sendTextData conn $
                WSProtocolServerMessageNewMessage $
                  WSNewMessage
                    { wsNewMessageMessage = newMessage
                    , wsNewMessageIsOwn = sessionClientPublicKey == decryptedMessageAuthorKey newMessage
                    }
            addSessionMessageHashRefs sessionID $ Set.singleton $ decryptedMessageHashRef newMessage
        MembersEvent{..} -> when (membersEventRefChan == activeChat) $ do
          liftIO $
            WS.sendTextData conn $
              WSProtocolServerMessageMembers $
                WSMembers
                  { wsMembersReaders = membersEventReaders
                  , wsMembersAuthors = membersEventAuthors
                  }

postHashRefToRefChan :: (MonadReader Env m, MonadUnliftIO m) => MyPublicKey -> MyRefChan -> MyHashRef -> m ()
postHashRefToRefChan author refChan hashRef = do
  creds <-
    liftIO $
      runKeymanClientRO $
        loadCredentials author >>= orThrow (RequestError "can't load credentials")
  -- creds contains message author keys, not peer keys, right?
  let box =
        makeSignedBox
          (_peerSignPk creds)
          (_peerSignSk creds)
          ( BSL.toStrict $
              serialise $
                AnnotatedHashRef Nothing $ -- we need 'AnnotatedHashRef' here so that peers download its content automatically
                  HashRef $
                    fromMyHashRef hashRef
          )
  refChanAPI <- asks refChanAPI
  void $ callService @RpcRefChanPropose refChanAPI (fromMyPublicKey refChan, box)

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
        (\session -> session{wsSessionActiveChat = Just chat})
        wsSessionID

addSessionMessageHashRefs :: (MonadReader Env m, MonadUnliftIO m) => WSSessionID -> Set MyHashRef -> m ()
addSessionMessageHashRefs wsSessionID messageHashRefs = do
  wsSessionsTVar' <- asks wsSessionsTVar
  atomically $
    modifyTVar wsSessionsTVar' $
      Map.adjust
        ( \session ->
            session
              { wsSessionMessages = Set.union (wsSessionMessages session) messageHashRefs
              }
        )
        wsSessionID
