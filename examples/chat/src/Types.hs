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

instance ToJSON Message where
  toJSON (Message {..}) =
    object
      [ "author" .= messageAuthor,
        "chat" .= messageChat,
        "body" .= messageBody,
        "createdAt" .= messageCreatedAt
      ]

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
  | WSProtocolMembers WSMembers

instance ToJSON WSProtocolMessage where
  toJSON (WSProtocolSubscribe _) = undefined
  toJSON (WSProtocolMessage _) = undefined
  toJSON (WSProtocolMessages msg) =
    object
      [ "type" .= ("messages" :: Text),
        "data"
          .= object
            [ "html" .= renderText (toHtml msg),
              "json" .= msg
            ]
      ]
  toJSON (WSProtocolMembers msg) =
    object
      [ "type" .= ("members" :: Text),
        "data" .= renderText (toHtml msg)
      ]

instance FromJSON WSProtocolMessage where
  parseJSON = withObject "WSProtocolMessage" $ \v -> do
    messageType <- v .: "type" :: Parser Text
    case messageType of
      "subscribe" -> WSProtocolSubscribe <$> parseJSON (Object v)
      "message" -> WSProtocolMessage <$> parseJSON (Object v)
      "messages" -> undefined
      _ -> fail $ "Unknown message type: " <> show messageType

instance WebSocketsData WSProtocolMessage where
  fromDataMessage (WS.Text _ (Just tl)) = orError "WSProtocolMessage decode error" $ Aeson.decode $ TLE.encodeUtf8 tl
  fromDataMessage (WS.Text bl Nothing) = orError "WSProtocolMessage decode error" $ Aeson.decode bl
  fromDataMessage (WS.Binary bl) = orError "WSProtocolMessage decode error" $ Aeson.decode bl
  fromLazyByteString = orError "WSProtocolMessage decode error" . Aeson.decode
  toLazyByteString = Aeson.encode

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

newtype WSMessages = WSMessages {fromWSMessages :: [Message]}
  deriving newtype (ToJSON)

instance ToHtml WSMessages where
  toHtml (WSMessages messages) = mapM_ toHtml messages
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
  { wsMembersReaders :: [ReaderMember],
    wsMembersAuthors :: [AuthorMember]
  }

instance ToHtml WSMembers where
  toHtml (WSMembers {..}) = do
    p_ "Authors"
    mapM_ toHtml wsMembersAuthors
    p_ [class_ "mt-1"] "Readers"
    mapM_ toHtml wsMembersReaders

  toHtmlRaw = toHtml

data ChatEvent
  = MessagesEvent MyRefChan
  | MembersEvent
      { membersEventRefChan :: MyRefChan,
        membersEventAuthors :: [AuthorMember],
        membersEventReaders :: [ReaderMember]
      }