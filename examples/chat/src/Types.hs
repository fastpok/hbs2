module Types where

import Codec.Serialise
import DBPipe.SQLite
import Data.Aeson hiding (encode, json)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.List qualified as L
import Data.Maybe
import Data.Set (Set)
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
import HBS2.Peer.Proto.Mailbox.Types
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

data NamedRefChan = NamedRefChan
  { namedRefChanKey :: MyRefChan
  , namedRefChanName :: Text
  }

newtype MyEncryptionPublicKey = MyEncryptionPublicKey {fromMyEncryptionPublicKey :: PubKey 'Encrypt 'HBS2Basic}
  deriving (Eq, Generic)
  deriving newtype (Serialise, FromStringMaybe)

instance Pretty (AsBase58 MyEncryptionPublicKey) where
  pretty (AsBase58 (MyEncryptionPublicKey k)) = pretty (AsBase58 k)

newtype MyHashRef = MyHashRef {fromMyHashRef :: Hash HbSync}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, Pretty, FromJSON)

instance ToField MyHashRef where
  toField x = toField $ show $ pretty x

instance FromField MyHashRef where
  fromField = fmap (MyHashRef . fromString @(Hash HbSync)) . fromField @String

type EncryptedMessage = Message 'HBS2Basic

data DecryptedMessage = DecryptedMessage
  { decryptedMessageHashRef :: MyHashRef
  , decryptedMessageAuthorKey :: MyPublicKey
  , decryptedMessageAuthorName :: Maybe Text
  , decryptedMessageChat :: MyRefChan
  , decryptedMessageBody :: Text
  , decryptedMessageCreatedAt :: UTCTime
  }

data MessageMetadata = MessageMetadata
  { messageMetaHashRef :: MyHashRef
  , messageMetaChat :: MyRefChan
  , messageMetaAuthor :: MyPublicKey
  , messageMetaCreatedAt :: UTCTime
  }
  deriving (Generic)

instance ToRow MessageMetadata where
  toRow MessageMetadata{..} =
    toRow
      ( messageMetaHashRef
      , messageMetaChat
      , messageMetaAuthor
      , messageMetaCreatedAt
      )

instance FromRow MessageMetadata where
  fromRow = do
    messageMetaHashRef <- field
    messageMetaChat <- field
    messageMetaAuthor <- field
    messageMetaCreatedAt <- field
    pure MessageMetadata{..}

data SpecialMessage = SpecialMessageSetName Text

type WSSessionID = UUID

data WSSession = WSSession
  { wsSessionConn :: WS.Connection
  , wsSessionClientSigil :: MySigil
  , wsSessionActiveChat :: Maybe MyRefChan
  , wsSessionMessages :: Set MyHashRef -- messages that were sent to the client
  }

data WSProtocolServerMessage
  = WSProtocolServerMessageOldMessages WSOldMessages
  | WSProtocolServerMessageNewMessage WSNewMessage
  | WSProtocolServerMessageMembers WSMembers
  | WSProtocolServerMessageName WSName

instance ToHtml WSProtocolServerMessage where
  toHtml (WSProtocolServerMessageOldMessages messages) = toHtml messages
  toHtml (WSProtocolServerMessageNewMessage message) = toHtml message
  toHtml (WSProtocolServerMessageMembers members) = toHtml members
  toHtml (WSProtocolServerMessageName name) = toHtml name
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

data Cursor = AfterCursor MyHashRef | BeforeCursor MyHashRef

instance ToField Cursor where
  toField (AfterCursor h) = toField h
  toField (BeforeCursor h) = toField h

data WSGetMessages = WSGetMessages
  { wsGetMessagesCursor :: MyHashRef
  , wsGetMessagesLimit :: Integer
  }

instance FromJSON WSGetMessages where
  parseJSON = withObject "WSMessage" $ \v -> do
    wsGetMessagesCursor <- v .: "cursor"
    wsGetMessagesLimit <- v .: "limit"
    pure $ WSGetMessages{..}

data WSOldMessagesHXSwap = WSOldMessagesHXSwapInnerHTML | WSOldMessagesHXSwapBeforeEnd

hxSwapToText :: WSOldMessagesHXSwap -> Text
hxSwapToText WSOldMessagesHXSwapInnerHTML = "innerHTML"
hxSwapToText WSOldMessagesHXSwapBeforeEnd = "beforeend"

data WSOldMessages = WSOldMessages
  { wsOldMessagesHXSwap :: WSOldMessagesHXSwap
  , wsOldMessages :: [DecryptedMessage]
  }

data WSNewMessage = WSNewMessage
  { wsNewMessageMessage :: DecryptedMessage
  , wsNewMessageIsOwn :: Bool
  }

data InfiniteScrollOpts = ApplyInfiniteScrollAttrs | DontApplyInfiniteScrollAttrs

pageSize :: Integer
pageSize = 20

hxValsScroll :: MyHashRef -> Text
hxValsScroll cursor =
  [qc|
\{
  "type": "get-messages",
  "cursor": "{show $ pretty cursor}",
  "limit": {show pageSize}
}
|]

messageToHTML :: (Monad m) => DecryptedMessage -> HtmlT m ()
messageToHTML DecryptedMessage{..} = do
  div_ [class_ "message-header"] $ do
    let authoreKeyText = T.pack $ show $ pretty $ AsBase58 decryptedMessageAuthorKey
        authorName = fromMaybe authoreKeyText decryptedMessageAuthorName
        createdAt = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" decryptedMessageCreatedAt
    div_ [class_ $ userNameToColorClass authoreKeyText] $ strong_ $ small_ [class_ "author-name"] $ toHtml authorName
    div_ $ small_ $ toHtml createdAt
  div_ [class_ "message-content"] $ do
    small_ $ sequence_ $ L.intersperse (br_ []) (toHtml <$> T.lines decryptedMessageBody)

oldMessageToHtml :: (Monad m) => InfiniteScrollOpts -> DecryptedMessage -> HtmlT m ()
oldMessageToHtml infiniteScrollOpts message@DecryptedMessage{..} =
  div_ ([class_ "message", data_ "author-key" authorKeyText] <> infiniteScrollAttrs) $
    messageToHTML message
 where
  cursor = decryptedMessageHashRef
  hxVals = hxValsScroll cursor
  infiniteScrollAttrs = case infiniteScrollOpts of
    ApplyInfiniteScrollAttrs ->
      [ wsSend_ ""
      , hxVals_ hxVals
      , hxTrigger_ "intersect once delay:200ms"
      , hxSwap_ "afterend"
      ]
    DontApplyInfiniteScrollAttrs -> []
  authorKeyText = T.pack $ show $ pretty $ AsBase58 $ decryptedMessageAuthorKey

newMessageToHtml :: (Monad m) => WSNewMessage -> HtmlT m ()
newMessageToHtml WSNewMessage{..} =
  div_ allAttrs $
    div_ [class_ "message", data_ "author-key" authorKeyText] $
      messageToHTML wsNewMessageMessage
 where
  attrs = [data_ "message-type" "new-message", hxSwapOOB_ "afterbegin:#messages"]
  allAttrs =
    if wsNewMessageIsOwn
      then data_ "own-message" "" : attrs
      else attrs
  authorKeyText = T.pack $ show $ pretty $ AsBase58 $ decryptedMessageAuthorKey wsNewMessageMessage

-- Applies first function to all elements except the last one.
-- Applies second function to the last element.
mapMLast_ :: (Monad m) => (a -> m b) -> (a -> m b) -> [a] -> m ()
mapMLast_ _ _ [] = pure ()
mapMLast_ _ fLast [x] = void $ fLast x
mapMLast_ fRest fLast (x : xs) = do
  _ <- fRest x
  mapMLast_ fRest fLast xs

instance ToHtml WSOldMessages where
  toHtml (WSOldMessages{..}) = div_
    [ id_ "messages"
    , hxSwapOOB_ $ hxSwapToText wsOldMessagesHXSwap
    , data_ "message-type" "old-messages"
    ]
    do
      mapMLast_
        (oldMessageToHtml DontApplyInfiniteScrollAttrs)
        (oldMessageToHtml ApplyInfiniteScrollAttrs)
        wsOldMessages
  toHtmlRaw = toHtml

instance ToHtml WSNewMessage where
  toHtml = newMessageToHtml
  toHtmlRaw = toHtml

data RefChanMembers = RefChanMembers
  { refChanMembersReaders :: [MyEncryptionPublicKey]
  , refChanMembersAuthors :: [MyPublicKey]
  }

data AuthorMember = AuthorMember
  { authorMemberKey :: MyPublicKey
  , authorMemberName :: Maybe Text
  }

instance ToHtml AuthorMember where
  toHtml (AuthorMember{..}) =
    let authorKeyText = T.pack $ show $ pretty $ AsBase58 authorMemberKey
        authorName = fromMaybe (shorten 8 authorKeyText) authorMemberName
     in p_ [class_ $ userNameToColorClass authorKeyText, data_ "author-key" authorKeyText] $ small_ [class_ "author-name"] $ toHtml authorName
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
  toHtml (WSMembers{..}) = div_ [id_ "members", hxSwapOOB_ "innerHTML", data_ "message-type" "members"] do
    mapM_ toHtml wsMembersAuthors
  toHtmlRaw = toHtml

data WSName = WSName
  { wsNameUserKey :: MyPublicKey
  , wsNameUserName :: Text
  }

instance ToHtml WSName where
  toHtml (WSName{..}) = do
    let authorKeyText = T.pack $ show $ pretty $ AsBase58 $ wsNameUserKey
        messagesOOB = "textContent:[data-author-key=\"" <> authorKeyText <> "\"] .author-name"
    div_ [hxSwapOOB_ messagesOOB, data_ "message-type" "name"] $ toHtml wsNameUserName
  toHtmlRaw = toHtml

data ChatEvent
  = MessagesEvent MyRefChan
  | MembersEvent
      { membersEventRefChan :: MyRefChan
      , membersEventAuthors :: [AuthorMember]
      , membersEventReaders :: [ReaderMember]
      }
  | NameEvent
      { nameEventRefChan :: MyRefChan
      , nameEventUserKey :: MyPublicKey
      , nameEventUserName :: Text
      }