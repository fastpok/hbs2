module Util.Text where

import Data.Text (Text)
import Data.Text qualified as Text

shorten :: Int -> Text -> Text
shorten n t =
  if Text.length t > n
    then Text.take m t <> "..." <> Text.takeEnd m t
    else t
  where
    m = n `div` 2