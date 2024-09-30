module Utils.Attributes
  ( ariaLabel_,
    ariaLive_,
  )
where

import Data.Text (Text)
import Lucid.Base

ariaLabel_ :: Text -> Attribute
ariaLabel_ = makeAttribute "aira-label"

ariaLive_ :: Text -> Attribute
ariaLive_ = makeAttribute "aira-live"