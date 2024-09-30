module Types where

import Codec.Serialise
import DBPipe.SQLite
import Data.Aeson hiding (encode, json)
import Data.Aeson.Decoding qualified as AD
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
  pretty (AsBase58 (MyPublicKey k)) = pretty (AsBase58 k)

instance FromField MyPublicKey where
  fromField = fmap fromString . fromField @String

instance ToField MyPublicKey where
  toField (MyPublicKey k) = toField $ show $ pretty (AsBase58 k)

type MyRefChan = MyPublicKey

newtype MyHash = MyHash {fromMyHash :: Hash HbSync}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, Pretty)

instance ToField MyHash where
  toField x = toField $ show $ pretty x

instance FromField MyHash where
  fromField = fmap (MyHash . fromString @(Hash HbSync)) . fromField @String

data Message = Message
  { messageAuthor :: MyPublicKey,
    messageChat :: MyRefChan,
    messageBody :: Text,
    messageCreatedAt :: UTCTime
  }
  deriving (Generic)

instance Serialise Message

instance ToRow Message where
  toRow msg@Message {..} = toRow (MyHash $ hashObject $ serialise msg, messageAuthor, messageChat, messageBody, messageCreatedAt)

instance FromRow Message where
  fromRow = do
    _hash :: MyHash <- field
    messageAuthor <- field
    messageChat <- field
    messageBody <- field
    messageCreatedAt <- field
    pure Message {..}

instance ToHtml Message where
  toHtml Message {..} = div_ [class_ "message"] $ do
    div_ [class_ "message-header"] $ do
      let author = T.pack $ show $ pretty $ AsBase58 messageAuthor
          createdAt = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" messageCreatedAt
      div_ [class_ $ userNameToColorClass author] $ strong_ $ small_ $ toHtml author
      div_ $ small_ $ toHtml createdAt
    div_ [class_ "message-content"] $ do
      small_ $ sequence_ $ L.intersperse (br_ []) (toHtml <$> T.lines messageBody)
  toHtmlRaw = toHtml

type WSSessionID = UUID

data WSSession = WSSession
  { wsSessionConn :: WS.Connection,
    wsSessionSubscriptions :: [MyRefChan]
  }

data WSProtocolMessage
  = WSProtocolSubscribe WSSubscribe
  | WSProtocolMessage WSMessage
  | WSProtocolMessages WSMessages

instance FromJSON WSProtocolMessage where
  parseJSON = withObject "WSProtocolMessage" $ \v -> do
    messageType <- v .: "type" :: Parser Text
    case messageType of
      "subscribe" -> WSProtocolSubscribe <$> parseJSON (Object v)
      "message" -> WSProtocolMessage <$> parseJSON (Object v)
      "messages" -> undefined
      _ -> fail $ "Unknown message type: " <> show messageType

instance WebSocketsData WSProtocolMessage where
  fromDataMessage (WS.Text _ (Just tl)) = orError "WSProtocolMessage decode error" $ AD.decode $ TLE.encodeUtf8 tl
  fromDataMessage (WS.Text bl Nothing) = orError "WSProtocolMessage decode error" $ AD.decode bl
  fromDataMessage (WS.Binary bl) = orError "WSProtocolMessage decode error" $ AD.decode bl
  fromLazyByteString = orError "WSProtocolMessage decode error" . AD.decode
  toLazyByteString (WSProtocolMessages messages) = TLE.encodeUtf8 $ renderText $ toHtml messages
  toLazyByteString _ = undefined

data WSSubscribe = WSSubscribe
  { wsSubscribeRefChan :: MyRefChan
  }

instance FromJSON WSSubscribe where
  parseJSON = withObject "WSSubscribe" $ \v -> do
    chat <- v .: "chat"
    pure $ WSSubscribe {wsSubscribeRefChan = chat}

data WSMessage = WSMessage
  { wsMessageChat :: MyRefChan,
    wsMessageAuthor :: MySigil,
    wsMessageBody :: Text
  }

instance FromJSON WSMessage where
  parseJSON = withObject "WSMessage" $ \v -> do
    chat <- v .: "chat"
    author <- v .: "author"
    body <- v .: "body"
    pure $
      WSMessage
        { wsMessageChat = chat,
          wsMessageAuthor = author,
          wsMessageBody = body
        }

data WSMessages = WSMessages [Message]

instance ToHtml WSMessages where
  toHtml (WSMessages messages) = mapM_ toHtml messages
  toHtmlRaw = toHtml
