module Error where

import Control.Exception (Exception (..))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Types
import Network.WebSockets
import Web.Scotty.Trans
import Web.Scotty.Trans qualified as Scotty

data Error = ServerError Text | RequestError Text
  deriving (Show, Eq)

instance Exception Error

errorText :: Error -> Text
errorText (ServerError t) = "Server error: " <> t
errorText (RequestError t) = "Request error: " <> t

instance ToJSON Error where
  toJSON e = object ["error" .= errorText e]

exceptionHandler :: (MonadIO m) => ErrorHandler m
exceptionHandler = Handler $ \case
  e@(ServerError _) -> do
    status status500
    Scotty.json e
  e@(RequestError _) -> do
    status status400
    Scotty.json e

data WSError
  = WSErrorBadHello
  | WSErrorBadMessage

wsErrorText :: WSError -> Text
wsErrorText WSErrorBadHello = "couldn't parse hello message"
wsErrorText WSErrorBadMessage = "couldn't parse message"

instance ToJSON WSError where
  toJSON e = object ["error" .= wsErrorText e]

instance WebSocketsData WSError where
  fromDataMessage = undefined
  fromLazyByteString = undefined
  toLazyByteString = Aeson.encode

orError :: String -> Maybe a -> a
orError s = fromMaybe (error s)
