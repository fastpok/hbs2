module Message where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Env
import HBS2.Data.Types.SignedBox
import HBS2.KeyMan.Keys.Direct
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.OrDie
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Client.Unix
import HBS2.Prelude (FromStringMaybe (..))
import Monad
import Types
import Web.Scotty.Trans

data Message = Message
  { messageAuthor :: MySigil,
    messageChat :: MyRefChan,
    messageBody :: Text
  }

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> do
    messageAuthor <-
      v .: "author"
        >>= \x -> parseSerialisableFromBase58 (BS8.pack x) `orFailWith` "Failed to parse sigil"
    messageChat <- v .: "chat" >>= \x -> fromStringMay x `orFailWith` "Failed to parse refchan"
    messageBody <- v .: "body"
    pure $ Message {..}

orFailWith :: Maybe a -> String -> Parser a
orFailWith maybeValue errorMsg = maybe (fail errorMsg) return maybeValue

postMessage :: ActionT AppM ()
postMessage = do
  message <- jsonData

  (sigilSignPublicKey, sigilData') <- orThrowUser "malformed sigil/bad signature" (unboxSignedBox0 (sigilData (messageAuthor message)))

  (creds, encKey_) <- liftIO $ runKeymanClient do
    -- what is creds here?
    creds <- loadCredentials sigilSignPublicKey >>= orThrowUser "can't load credentials"
    encKey <- loadKeyRingEntry (sigilDataEncKey @'HBS2Basic sigilData')
    pure (creds, encKey)

  let box = makeSignedBox (_peerSignPk creds) (_peerSignSk creds) (TE.encodeUtf8 $ messageBody message)

  refChanAPI <- asks refChanAPI
  void $ callService @RpcRefChanPropose refChanAPI (messageChat message, box)
