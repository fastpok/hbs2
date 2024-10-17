module Types where

import Codec.Serialise
import DBPipe.SQLite
import Data.Aeson hiding (encode, json)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time
import Data.UUID (UUID)
import Error
import HBS2.Base58
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Prelude
import Lucid
import Network.WebSockets (WebSocketsData)
import Network.WebSockets qualified as WS
import Text.InterpolatedString.Perl6 (qc)
import Util.Attributes
import Util.Text
import Util.UserNameColor

newtype MySigil = MySigil {fromMySigil :: Sigil 'HBS2Basic}
  deriving (Generic)
  deriving newtype (Serialise)

instance FromJSON MySigil where
  parseJSON = withText "MySigil" $ \t -> do
    case parseSerialisableFromBase58 $ TE.encodeUtf8 t of
      Nothing -> fail "couldn't parse sigil"
      Just sigil -> pure sigil

newtype MyPublicKey = MyPublicKey {fromMyPublicKey :: PubKey 'Sign 'HBS2Basic}
  deriving (Eq, Generic)
  deriving newtype (Serialise, FromStringMaybe)

instance FromJSON MyPublicKey where
  parseJSON = withText "MyPublicKey" $ \t -> do
    case fromStringMay $ T.unpack t of
      Nothing -> fail "couldn't parse public key"
      Just publicKey -> pure publicKey

instance IsString MyPublicKey where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 MyPublicKey) where
  pretty (AsBase58 (MyPublicKey k)) = pretty $ AsBase58 k

instance FromField MyPublicKey where
  fromField = fmap fromString . fromField @String

instance ToField MyPublicKey where
  toField (MyPublicKey k) = toField $ show $ pretty $ AsBase58 k

instance ToJSON MyPublicKey where
  toJSON (MyPublicKey k) = toJSON $ show $ pretty $ AsBase58 k

type MyRefChan = MyPublicKey

newtype MyEncryptionPublicKey = MyEncryptionPublicKey {fromMyEncryptionPublicKey :: PubKey 'Encrypt 'HBS2Basic}
  deriving (Eq, Generic)
  deriving newtype (Serialise, FromStringMaybe)

instance Pretty (AsBase58 MyEncryptionPublicKey) where
  pretty (AsBase58 (MyEncryptionPublicKey k)) = pretty (AsBase58 k)

newtype MyHash = MyHash {fromMyHash :: Hash HbSync}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, Pretty, FromJSON)

instance ToField MyHash where
  toField x = toField $ show $ pretty x

instance FromField MyHash where
  fromField = fmap (MyHash . fromString @(Hash HbSync)) . fromField @String

data Message = Message
  { messageAuthor :: MyPublicKey
  , messageChat :: MyRefChan
  , messageBody :: Text
  , messageCreatedAt :: UTCTime
  }
  deriving (Generic)

instance Serialise Message

instance ToRow Message where
  toRow msg@Message{..} = toRow (MyHash $ hashObject $ serialise msg, messageAuthor, messageChat, messageBody, messageCreatedAt)

instance FromRow Message where
  fromRow = do
    _hash :: MyHash <- field
    messageAuthor <- field
    messageChat <- field
    messageBody <- field
    messageCreatedAt <- field
    pure Message{..}

type WSSessionID = UUID

data WSSession = WSSession
  { wsSessionConn :: WS.Connection
  , wsSessionClientSigil :: MySigil
  , wsSessionActiveChat :: Maybe MyRefChan
  }

data WSProtocolServerMessage
  = WSProtocolServerMessageMessages WSMessages
  | WSProtocolServerMessageMembers WSMembers

instance ToHtml WSProtocolServerMessage where
  toHtml (WSProtocolServerMessageMessages messages) = toHtml messages
  toHtml (WSProtocolServerMessageMembers members) = toHtml members
  toHtmlRaw = toHtml

instance WebSocketsData WSProtocolServerMessage where
  fromDataMessage = undefined
  fromLazyByteString = undefined
  toLazyByteString = TLE.encodeUtf8 . renderText . toHtml

data WSProtocolClientMessage
  = WSProtocolClientMessageHello WSHello
  | WSProtocolClientMessageActiveChat WSActiveChat
  | WSProtocolClientMessageMessage WSMessage
  | WSProtocolClientMessageGetMessages WSGetMessages

instance FromJSON WSProtocolClientMessage where
  parseJSON = withObject "WSProtocolClientMessage" $ \v -> do
    messageType <- v .: "type" :: Parser Text
    case messageType of
      "hello" -> WSProtocolClientMessageHello <$> parseJSON (Object v)
      "active-chat" -> WSProtocolClientMessageActiveChat <$> parseJSON (Object v)
      "message" -> WSProtocolClientMessageMessage <$> parseJSON (Object v)
      "get-messages" -> WSProtocolClientMessageGetMessages <$> parseJSON (Object v)
      _ -> fail $ "Unknown message type: " <> show messageType

instance WebSocketsData WSProtocolClientMessage where
  fromDataMessage (WS.Text _ (Just tl)) = orError "WSProtocolClientMessage decode error" $ Aeson.decode $ TLE.encodeUtf8 tl
  fromDataMessage (WS.Text bl Nothing) = orError "WSProtocolClientMessage decode error" $ Aeson.decode bl
  fromDataMessage (WS.Binary bl) = orError "WSProtocolClientMessage decode error" $ Aeson.decode bl
  fromLazyByteString = orError "WSProtocolClientMessage decode error" . Aeson.decode
  toLazyByteString = undefined

newtype WSHello = WSHello
  { wsHelloClientSigil :: MySigil
  }

instance FromJSON WSHello where
  parseJSON = withObject "WSHello" $ \v -> do
    client <- v .: "client"
    pure $ WSHello{wsHelloClientSigil = client}

newtype WSActiveChat = WSActiveChat
  { fromWSActiveChat :: MyRefChan
  }

instance FromJSON WSActiveChat where
  parseJSON = withObject "WSActiveChat" $ \v -> do
    chat <- v .: "chat"
    pure $ WSActiveChat chat

newtype WSMessage = WSMessage
  { wsMessage :: Text
  }

instance FromJSON WSMessage where
  parseJSON = withObject "WSMessage" $ \v -> do
    message <- v .: "message"
    pure $ WSMessage message

data WSGetMessages = WSGetMessages
  { wsGetMessagesCursor :: MyHash
  , wsGetMessagesLimit :: Integer
  }

instance FromJSON WSGetMessages where
  parseJSON = withObject "WSMessage" $ \v -> do
    wsGetMessagesCursor <- v .: "cursor"
    wsGetMessagesLimit <- v .: "limit"
    pure $ WSGetMessages{..}

newtype WSMessages = WSMessages {fromWSMessages :: [Message]}

data InfiniteScrollOpts = ApplyInfiniteScrollAttrs | DontApplyInfiniteScrollAttrs

pageSize :: Integer
pageSize = 20

hxValsScroll :: MyHash -> Text
hxValsScroll cursor =
  [qc|
\{
  "type": "get-messages",
  "cursor": "{pretty cursor}",
  "limit": {show pageSize}
}
|]

messageToHtml :: (Monad m) => InfiniteScrollOpts -> Message -> HtmlT m ()
messageToHtml applyInfiniteScrollAttrs msg@Message{..} =
  let cursor = MyHash $ hashObject $ serialise msg
      hxVals = hxValsScroll cursor
      infiniteScrollAttrs = case applyInfiniteScrollAttrs of
        ApplyInfiniteScrollAttrs ->
          [ wsSend_ ""
          , hxVals_ hxVals
          , hxTrigger_ "intersect once"
          , hxSwap_ "afterend"
          ]
        DontApplyInfiniteScrollAttrs -> []
   in div_ ([class_ "message"] <> infiniteScrollAttrs) $ do
        div_ [class_ "message-header"] $ do
          let author = T.pack $ show $ pretty $ AsBase58 messageAuthor
              createdAt = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" messageCreatedAt
          div_ [class_ $ userNameToColorClass author] $ strong_ $ small_ $ toHtml author
          div_ $ small_ $ toHtml createdAt
        div_ [class_ "message-content"] $ do
          small_ $ sequence_ $ L.intersperse (br_ []) (toHtml <$> T.lines messageBody)

-- Applies first function to all elements except the last one.
-- Applies second function to the last element.
mapMLast_ :: (Monad m) => (a -> m b) -> (a -> m b) -> [a] -> m ()
mapMLast_ _ _ [] = pure ()
mapMLast_ _ fLast [x] = void $ fLast x
mapMLast_ fRest fLast (x : xs) = do
  _ <- fRest x
  mapMLast_ fRest fLast xs

instance ToHtml WSMessages where
  toHtml (WSMessages messages) = div_ [id_ "messages", hxSwapOob_ "beforeend"] do
    mapMLast_
      (messageToHtml DontApplyInfiniteScrollAttrs)
      (messageToHtml ApplyInfiniteScrollAttrs)
      messages
  toHtmlRaw = toHtml

newtype AuthorMember = AuthorMember {fromAuthorMember :: MyPublicKey}

instance ToHtml AuthorMember where
  toHtml (AuthorMember key) =
    let username = T.pack $ show $ pretty $ AsBase58 key
     in p_ [class_ $ userNameToColorClass username] $ small_ $ toHtml $ shorten 8 username
  toHtmlRaw = toHtml

newtype ReaderMember = ReaderMember {fromReaderMember :: MyEncryptionPublicKey}

instance ToHtml ReaderMember where
  toHtml (ReaderMember key) =
    let username = T.pack $ show $ pretty $ AsBase58 key
     in p_ [class_ $ userNameToColorClass username] $ small_ $ toHtml $ shorten 8 username
  toHtmlRaw = toHtml

data WSMembers = WSMembers
  { wsMembersReaders :: [ReaderMember]
  , wsMembersAuthors :: [AuthorMember]
  }

instance ToHtml WSMembers where
  toHtml (WSMembers{..}) = div_ [id_ "members", hxSwapOob_ "innerHTML"] do
    p_ "Authors"
    mapM_ toHtml wsMembersAuthors
    p_ [class_ "mt-1"] "Readers"
    mapM_ toHtml wsMembersReaders

  toHtmlRaw = toHtml

data ChatEvent
  = MessagesEvent MyRefChan
  | MembersEvent
      { membersEventRefChan :: MyRefChan
      , membersEventAuthors :: [AuthorMember]
      , membersEventReaders :: [ReaderMember]
      }