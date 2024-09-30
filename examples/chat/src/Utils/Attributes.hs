module Utils.Attributes (
  ariaLabel_,
  ariaLive_,
  hyper_,
)
where

import Data.Text (Text)
import Lucid.Base

ariaLabel_ :: Text -> Attribute
ariaLabel_ = makeAttribute "aira-label"

ariaLive_ :: Text -> Attribute
ariaLive_ = makeAttribute "aira-live"

hyper_ :: Text -> Attribute
hyper_ = makeAttribute "_"