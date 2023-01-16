{-# Language TypeFamilyDependencies  #-}
{-# Language FunctionalDependencies  #-}
module HasProtocol where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

class HasPeer p where
  type family (Peer p) :: Type

class HasPeer p => HasProtocol p a  | a -> p where
  type family ProtocolId a = (id :: Nat) | id -> a
  type family Encoded p :: Type

  protoId :: forall . KnownNat (ProtocolId a) => Proxy a -> Integer
  protoId _ = natVal (Proxy @(ProtocolId a))

  decode :: Encoded p -> Maybe a
  encode :: a -> Encoded p


