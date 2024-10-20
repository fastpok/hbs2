{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module HBS2.Net.Proto.RefLog where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
import HBS2.Events
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs

import HBS2.System.Logger.Simple

import Data.Maybe
import Data.Hashable
import Data.ByteString (ByteString)
import Type.Reflection (someTypeRep)
import Lens.Micro.Platform

data RefLogRequest e =
    RefLogRequest  (PubKey 'Sign (Encryption e))
  | RefLogResponse (PubKey 'Sign (Encryption e)) (Hash HbSync)
  deriving stock (Generic)

data RefLogUpdate e =
  RefLogUpdate
  { _refLogId           :: PubKey 'Sign (Encryption e)
  , _refLogUpdNonce     :: Nonce (RefLogUpdate e)
  , _refLogUpdData      :: ByteString
  , _refLogUpdSign      :: Signature (Encryption e)
  }
  deriving stock (Generic)

makeLenses 'RefLogUpdate

newtype RefLogUpdateI e m =
  RefLogUpdateI
  { refLogBroadcast  :: RefLogUpdate e -> m ()
  }

data RefLogUpdateEv e
data RefLogRequestAnswer e

data instance EventKey e (RefLogUpdateEv e) =
  RefLogUpdateEvKey
  deriving (Generic,Typeable,Eq)

instance Typeable (RefLogUpdateEv e) => Hashable (EventKey e (RefLogUpdateEv e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @RefLogUpdateEv

newtype instance Event e (RefLogUpdateEv e) =
  RefLogUpdateEvData (PubKey 'Sign (Encryption e), RefLogUpdate e)
  deriving (Typeable)

instance EventType ( Event e (RefLogUpdateEv e) ) where
  isPersistent = True

instance Expires (EventKey e (RefLogUpdateEv e)) where
  expiresIn = const Nothing

data instance EventKey e (RefLogRequestAnswer e) =
  RefLogReqAnswerKey
  deriving stock (Generic,Typeable,Eq)

instance Typeable (RefLogRequestAnswer e) => Hashable (EventKey e (RefLogRequestAnswer e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @(RefLogRequestAnswer e)

data instance Event e (RefLogRequestAnswer e) =
  RefLogReqAnswerData (PubKey 'Sign (Encryption e)) (Hash HbSync)
  deriving (Typeable)

instance EventType ( Event e (RefLogRequestAnswer e) ) where
  isPersistent = True

instance Expires (EventKey e (RefLogRequestAnswer e)) where
  expiresIn = const Nothing

makeRefLogUpdate :: forall e s m . ( MonadIO m
                                   , HasNonces (RefLogUpdate e) m
                                   , Nonce (RefLogUpdate e) ~ ByteString
                                   , Signatures s
                                   , s ~ Encryption e
                                   , IsRefPubKey s
                                   )
                 => PubKey 'Sign s
                 -> PrivKey 'Sign s
                 -> ByteString
                 -> m (RefLogUpdate e)

makeRefLogUpdate pubk privk bs = do
  nonce <- newNonce @(RefLogUpdate e)
  let noncebs = nonce <> bs
  let sign = makeSign @s privk noncebs
  pure $ RefLogUpdate pubk nonce bs sign

verifyRefLogUpdate :: forall e s m . ( MonadIO m
                                     , Nonce (RefLogUpdate e) ~ ByteString
                                     , Signatures s
                                     , s ~ Encryption e
                                     )
                 => RefLogUpdate e -> m Bool
verifyRefLogUpdate msg = do
  let pubk = view refLogId msg
  let noncebs = view refLogUpdNonce msg <> view refLogUpdData msg
  let sign = view refLogUpdSign msg
  pure $ verifySign @s pubk sign noncebs

data RefLogRequestI e m =
  RefLogRequestI
  { onRefLogRequest :: (Peer e, PubKey 'Sign (Encryption e)) -> m (Maybe (Hash HbSync))
  , onRefLogResponse :: (Peer e, PubKey 'Sign (Encryption e), Hash HbSync) -> m ()
  }

refLogRequestProto :: forall e s m . ( MonadIO m
                                     , Request e (RefLogRequest e) m
                                     , Response e (RefLogRequest e) m
                                     , HasDeferred e (RefLogRequest e) m
                                     , Sessions e (KnownPeer e) m
                                     , IsPeerAddr e m
                                     , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
                                     , EventEmitter e (RefLogRequestAnswer e) m
                                     , Pretty (Peer e)
                                     , s ~ Encryption e
                                     )
                  => RefLogRequestI e m -> RefLogRequest e -> m ()

refLogRequestProto adapter cmd = do

  p <- thatPeer proto
  auth <- find (KnownPeerKey p) id <&> isJust

  when auth do

    -- FIXME: asap-only-accept-response-if-we-have-asked

    case cmd of
      (RefLogRequest pk) -> do
         trace $ "got RefLogUpdateRequest for" <+> pretty (AsBase58 pk)
         pip <- thatPeer proto
         answ' <- onRefLogRequest adapter (pip,pk)
         maybe1 answ' none $ \answ -> do
          response (RefLogResponse @e pk answ)

      (RefLogResponse pk h) -> do
         trace $ "got RefLogResponse for" <+> pretty (AsBase58 pk) <+> pretty h
         pip <- thatPeer proto
         emit RefLogReqAnswerKey (RefLogReqAnswerData @e pk h)
         onRefLogResponse adapter (pip,pk,h)

  where
    proto = Proxy @(RefLogRequest e)

refLogUpdateProto :: forall e s m . ( MonadIO m
                                    , Request e (RefLogUpdate e) m
                                    , Response e (RefLogUpdate e) m
                                    , HasDeferred e (RefLogUpdate e) m
                                    , IsPeerAddr e m
                                    , Pretty (Peer e)
                                    , Nonce (RefLogUpdate e) ~ ByteString
                                    , Sessions e (KnownPeer e) m
                                    , Signatures s
                                    , Pretty (AsBase58 (PubKey 'Sign s))
                                    , EventEmitter e (RefLogUpdateEv e) m
                                    , s ~ Encryption e
                                    )
                  => RefLogUpdateI e m -> RefLogUpdate e -> m ()

refLogUpdateProto adapter =
  \case
    e@RefLogUpdate{} -> do
      p <- thatPeer proto
      auth <- find (KnownPeerKey p) id <&> isJust

      when auth do

        let pubk = view refLogId e
        trace $ "got RefLogUpdate for" <+> pretty (AsBase58 pubk)
        signed <- verifyRefLogUpdate @e e

        when signed do
          trace "RefLogUpdate is signed properly"

          -- FIXME: refactor:use-type-application-for-deferred
          deferred proto do
            emit @e RefLogUpdateEvKey (RefLogUpdateEvData (pubk, e))
            refLogBroadcast adapter e
            pure ()

    where
      proto = Proxy @(RefLogUpdate e)

instance  ( Serialise (PubKey 'Sign (Encryption e))
          , Serialise (Nonce (RefLogUpdate e))
          , Serialise (Signature (Encryption e))
          ) => Serialise (RefLogUpdate e)


instance  ( Serialise (PubKey 'Sign (Encryption e))
          ) => Serialise (RefLogRequest e)

