module Util.Lucid.Elements where

import Lucid.Base

dialog_ :: (Term arg result) => arg -> result
dialog_ = term "dialog"