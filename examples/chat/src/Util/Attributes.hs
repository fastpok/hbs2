module Util.Attributes
  ( ariaLabel_,
    ariaLive_,
    hyper_,
    hxExt_,
    wsConnect_,
    wsSend_,
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

hxExt_ :: Text -> Attribute
hxExt_ = makeAttribute "hx-ext"

wsConnect_ :: Text -> Attribute
wsConnect_ = makeAttribute "ws-connect"

wsSend_ :: Text -> Attribute
wsSend_ = makeAttribute "ws-send"