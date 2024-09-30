module Error
  ( Error (..),
    exceptionHandler,
  )
where

import Control.Exception (Exception (..))
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Text (Text)
import Network.HTTP.Types
import Web.Scotty.Trans

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
    json e
  e@(RequestError _) -> do
    status status400
    json e