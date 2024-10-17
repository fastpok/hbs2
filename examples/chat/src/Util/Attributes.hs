module Util.Attributes
  ( ariaLabel_,
    ariaLive_,
    hyper_,
    hxTrigger_,
    hxVals_,
    hxParams_,
    hxDisinherit_,
    hxSwapOob_,
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

hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

hxVals_ :: Text -> Attribute
hxVals_ = makeAttribute "hx-vals"

hxParams_ :: Text -> Attribute
hxParams_ = makeAttribute "hx-params"

hxDisinherit_ :: Text -> Attribute
hxDisinherit_ = makeAttribute "hx-disinherit"

hxSwapOob_ :: Text -> Attribute
hxSwapOob_ = makeAttribute "hx-swap-oob"

hxExt_ :: Text -> Attribute
hxExt_ = makeAttribute "hx-ext"

wsConnect_ :: Text -> Attribute
wsConnect_ = makeAttribute "ws-connect"

wsSend_ :: Text -> Attribute
wsSend_ = makeAttribute "ws-send"