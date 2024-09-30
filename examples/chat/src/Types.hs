module Types where

import Codec.Serialise
import Control.Monad
import DBPipe.SQLite
import Data.Aeson hiding (encode, json)
import Data.Aeson qualified as Aeson
import Data.Aeson.Decoding qualified as AD
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 qualified as BS8
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time
import Data.UUID (UUID)
import HBS2.Base58
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Prelude
import Lucid
import Network.WebSockets (WebSocketsData)
import Network.WebSockets qualified as WS
import Util.UserNameColor

type MySigil = Sigil 'HBS2Basic

newtype MyPublicKey = MyPublicKey {fromMyPublicKey :: PubKey 'Sign 'HBS2Basic}
  deriving (Eq, Ord, Show, Generic)

instance Serialise MyPublicKey

instance FromStringMaybe MyPublicKey where
  fromStringMay s = MyPublicKey <$> fromStringMay s

instance IsString MyPublicKey where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 MyPublicKey) where
  pretty (AsBase58 (MyPublicKey k)) = pretty (AsBase58 k)

instance FromField MyPublicKey where
  fromField = fmap fromString . fromField @String

instance ToField MyPublicKey where
  toField (MyPublicKey k) = toField $ show $ pretty (AsBase58 k)

type MyRefChan = MyPublicKey

data Message = Message
  { messageAuthor :: MyPublicKey,
    messageChat :: MyRefChan,
    messageBody :: Text,
    messageCreatedAt :: UTCTime
  }
  deriving (Generic)

instance Serialise Message

instance ToHtml Message where
  toHtml Message {..} =
    let author = T.pack $ show $ pretty $ AsBase58 messageAuthor
        createdAt = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" messageCreatedAt
     in createMessage author messageBody createdAt

  -- TODO: fix this
  toHtmlRaw = toHtml

instance FromRow Message where
  fromRow = do
    _hash :: MyHash <- field
    messageAuthor <- field
    messageChat <- field
    messageBody <- field
    messageCreatedAt <- field
    pure Message {..}

instance ToJSON Message where
  toJSON Message {..} =
    object
      [ "author" .= show (pretty $ AsBase58 messageAuthor),
        "chat" .= show (pretty $ AsBase58 messageChat),
        "body" .= messageBody,
        "created_at" .= messageCreatedAt
      ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> do
    messageAuthor <- v .: "author" <&> fromString
    messageChat <- v .: "chat" <&> fromString
    messageBody <- v .: "body"
    messageCreatedAt <- v .: "created_at"
    pure $ Message {..}

instance WebSocketsData Message where
  fromDataMessage (WS.Text _ (Just tl)) = orError $ AD.decode $ TLE.encodeUtf8 tl
  fromDataMessage (WS.Text bl Nothing) = orError $ AD.decode bl
  fromDataMessage (WS.Binary bl) = orError $ AD.decode bl
  fromLazyByteString = orError . AD.decode
  toLazyByteString = TLE.encodeUtf8 . renderText . toHtml

newtype MyHash = MyHash {fromMyHash :: Hash HbSync}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, Pretty)

instance ToField MyHash where
  toField x = toField $ show $ pretty x

instance FromField MyHash where
  fromField = fmap (MyHash . fromString @(Hash HbSync)) . fromField @String

instance ToRow Message where
  toRow msg@Message {..} = toRow (MyHash $ hashObject $ serialise msg, messageChat, messageAuthor, messageBody, messageCreatedAt)

newtype Messages = Messages
  { fromMessages :: [Message]
  }

instance ToJSON Messages where
  toJSON (Messages msgs) = toJSON msgs

instance FromJSON Messages where
  parseJSON v = Messages <$> parseJSON v

instance WS.WebSocketsData Messages where
  fromDataMessage (WS.Text _ (Just tl)) = orError $ AD.decode $ TLE.encodeUtf8 tl
  fromDataMessage (WS.Text bl Nothing) = orError $ AD.decode bl
  fromDataMessage (WS.Binary bl) = orError $ AD.decode bl
  fromLazyByteString = orError . AD.decode
  toLazyByteString messages = TLE.encodeUtf8 $ renderText $ do
    forM_ (fromMessages messages) toHtml

createMessage :: (Monad m) => Text -> Text -> Text -> HtmlT m ()
createMessage author message time =
  div_ [class_ "message"] $ do
    div_ [class_ "message-header"] $ do
      div_ [class_ $ userNameToColorClass author] $ strong_ $ small_ $ toHtml author
      div_ $ small_ $ toHtml time
    div_ [class_ "message-content"] $ do
      small_ $ sequence_ $ L.intersperse (br_ []) (toHtml <$> T.lines message)

data WSClient = WSClient
  { wsClientKey :: MyPublicKey,
    wsClientConn :: WS.Connection,
    wsClientSubscriptions :: [MyRefChan],
    wsClientID :: UUID
  }

data MessageReq = MessageReq
  { messageReqAuthor :: MySigil,
    messageReqChat :: MyRefChan,
    messageReqBody :: Text
  }

instance FromJSON MessageReq where
  parseJSON = withObject "MessageReq" $ \v -> do
    messageReqAuthor <-
      v .: "author"
        >>= \x -> parseSerialisableFromBase58 (BS8.pack x) `orFailWith` "couldn't parse sigil"
    messageReqChat <- v .: "chat" <&> fromString
    messageReqBody <- v .: "body"
    pure $ MessageReq {..}

instance ToJSON MessageReq where
  toJSON MessageReq {..} =
    object
      [ "author" .= show (pretty $ AsBase58 messageReqAuthor),
        "body" .= messageReqBody
      ]

instance WebSocketsData MessageReq where
  fromDataMessage (WS.Text _ (Just tl)) = orError $ AD.decode $ TLE.encodeUtf8 tl
  fromDataMessage (WS.Text bl Nothing) = orError $ AD.decode bl
  fromDataMessage (WS.Binary bl) = orError $ AD.decode bl
  fromLazyByteString = orError . AD.decode
  toLazyByteString = Aeson.encode

orFailWith :: Maybe a -> String -> Parser a
orFailWith maybeValue errorMsg = maybe (fail errorMsg) return maybeValue

orError :: Maybe a -> a
orError = fromMaybe (error "couldn't decode MessageReq")