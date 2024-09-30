{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import Crypto.Saltine.Core.Sign qualified as Sign
import Data.Text.Lazy qualified as TL
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Prelude
import Web.Scotty.Trans

type MySigil = Sigil 'HBS2Basic

type MyRefChan = PubKey 'Sign 'HBS2Basic

instance Parsable Sign.PublicKey where
  parseParam = maybe (Left "Failed to parse refchan") Right . fromStringMay . TL.unpack
