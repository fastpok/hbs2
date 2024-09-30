module Message where

import Codec.Serialise
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (encode, json)
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.List (sortOn)
import Data.Text qualified as T
import Data.Time
import Env
import Error
import HBS2.Base58
import HBS2.Data.Types
import HBS2.Data.Types.SignedBox
import HBS2.KeyMan.Keys.Direct
import HBS2.Merkle
import HBS2.Net.Auth.Credentials hiding (encode)
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.OrDie
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.RPC.Client.Unix hiding (encode)
import HBS2.Prelude
import HBS2.Storage
import Lucid hiding (for_)
import Monad
import Streaming.Prelude qualified as S
import Types
import Util.UserNameColor
import Web.Scotty.Trans

data Message = Message
  { messageAuthor :: MySigil,
    messageBody :: Text
  }

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> do
    messageAuthor <-
      v .: "author"
        >>= \x -> parseSerialisableFromBase58 (BS8.pack x) `orFailWith` "Failed to parse sigil"
    messageBody <- v .: "body"
    pure $ Message {..}

data MessageResp = MessageResp
  { messageRespAuthor :: PubKey 'Sign 'HBS2Basic,
    messageRespBody :: Text,
    messageRespTime :: UTCTime
  }
  deriving (Generic)

instance Serialise MessageResp

instance ToHtml MessageResp where
  toHtml MessageResp {..} = createMessage (T.pack (show $ pretty $ AsBase58 messageRespAuthor)) messageRespBody (T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" messageRespTime)

createMessage :: (Monad m) => Text -> Text -> Text -> HtmlT m ()
createMessage author message time =
  div_ [class_ "message"] $ do
    div_ [class_ "message-header"] $ do
      div_ [class_ $ userNameToColorClass author] $ strong_ $ small_ $ toHtml author
      div_ $ small_ $ toHtml time
    div_ [class_ "message-content"] $ do
      small_ $ toHtml message

orFailWith :: Maybe a -> String -> Parser a
orFailWith maybeValue errorMsg = maybe (fail errorMsg) return maybeValue

postMessage :: ActionT AppM ()
postMessage = do
  message <- jsonData
  refChan <- pathParam "chat"

  (sigilSignPublicKey, sigilData') <- orThrow (RequestError "malformed sigil/bad signature") (unboxSignedBox0 (sigilData (messageAuthor message)))

  (creds, encKey_) <- liftIO $ runKeymanClient do
    -- what is creds here?
    creds <- loadCredentials sigilSignPublicKey >>= orThrow (RequestError "can't load credentials")
    encKey <- loadKeyRingEntry (sigilDataEncKey @'HBS2Basic sigilData')
    pure (creds, encKey)

  time <- liftIO getCurrentTime
  let msg =
        MessageResp
          { messageRespAuthor = sigilSignPk $ messageAuthor message,
            messageRespBody = messageBody message,
            messageRespTime = time
          }
  let box = makeSignedBox (_peerSignPk creds) (_peerSignSk creds) (BSL.toStrict $ serialise msg)

  refChanAPI <- asks refChanAPI
  void $ callService @RpcRefChanPropose refChanAPI (refChan, box)

getMessages :: ActionT AppM ()
getMessages = do
  refChan <- pathParam "chat"
  refChanAPI <- asks refChanAPI
  storageAPI <- asks storageAPI

  hashRef <-
    callService @RpcRefChanGet refChanAPI refChan
      >>= orThrow (ServerError "can't request refchan")
      >>= orThrow (ServerError "refchan not found")
  let storage = AnyStorage (StorageClient storageAPI)

  messages <- S.toList_ $ walkMerkle (fromHashRef hashRef) (getBlock storage) $ \case
    Left {} -> pure ()
    Right hashRefs -> do
      for_ hashRefs $ \h -> void $ runMaybeT do
        s <-
          getBlock storage (fromHashRef h)
            >>= toMPlus
              <&> deserialiseOrFail @(RefChanUpdate L4Proto)
            >>= toMPlus
        case s of
          Accept {} -> pure ()
          Propose _ box -> do
            -- is this really author's key ?
            (peerKey_, ProposeTran _ pbox :: ProposeTran L4Proto) <- toMPlus $ unboxSignedBox0 box
            -- then what key is this?
            (authorKey_, bs) <- toMPlus $ unboxSignedBox0 pbox
            case deserialiseOrFail $ BSL.fromStrict bs of
              Left _ -> do
                pure ()
              Right msg ->
                lift $
                  S.yield msg
  html $ mconcat $ renderText . toHtml <$> sortOn messageRespTime messages
