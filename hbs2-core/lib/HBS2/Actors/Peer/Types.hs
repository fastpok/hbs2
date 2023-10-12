{-# Language AllowAmbiguousTypes #-}
module HBS2.Actors.Peer.Types where

import HBS2.Prelude
import HBS2.Storage
import HBS2.Net.Proto.Types
import HBS2.Net.Messaging
import HBS2.Hash

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)


class HasProtocol e p => HasTimeLimits e p m where
  tryLockForPeriod :: Peer e -> p -> m Bool

instance {-# OVERLAPPABLE #-}
  (MonadIO (t m), Monad m, MonadTrans t, HasProtocol e p, HasTimeLimits e p m) => HasTimeLimits e p (t m) where
  tryLockForPeriod p m = lift (tryLockForPeriod p m)
    -- pure True
    -- liftIO $ print "LIMIT DOES NOT WORK"
    -- pure True

-- instance HasConf m => HasConf (ResponseM e m)


instance (IsKey HbSync, MonadIO m) => Storage AnyStorage HbSync ByteString m  where
  putBlock (AnyStorage s) = liftIO . putBlock s
  enqueueBlock (AnyStorage s) = liftIO . enqueueBlock s
  getBlock (AnyStorage s) = liftIO . getBlock s
  getChunk (AnyStorage s) h a b = liftIO $ getChunk s h a b
  hasBlock (AnyStorage s) = liftIO . hasBlock s
  updateRef (AnyStorage s) r v = liftIO $ updateRef s r v
  getRef (AnyStorage s) = liftIO . getRef s
  delBlock (AnyStorage s) = liftIO . delBlock s
  delRef (AnyStorage s) = liftIO . delRef s

data AnyStorage = forall zu  . ( Storage zu HbSync ByteString IO
                               ) => AnyStorage zu

class HasStorage m where
  getStorage :: m AnyStorage


instance (Monad m, HasStorage m) => HasStorage (MaybeT m) where
  getStorage = lift getStorage


class (Monad m, HasProtocol e p) => HasGossip e p m where
  gossip :: p -> m ()


class Monad m => HasOwnPeer e m where
  ownPeer :: m (Peer e)


data Fabriq e = forall bus . (Messaging bus e (Encoded e)) => Fabriq bus

class HasFabriq e m where
  getFabriq :: m (Fabriq e)


