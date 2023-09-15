{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language MultiWayIf #-}
module PeerTypes where

import HBS2.Actors.Peer
import HBS2.Actors.Peer.Types
import HBS2.Clock
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types.Peer
import HBS2.Data.Types.Refs
import HBS2.Data.Detect
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle (AnnMetaData)
import HBS2.Net.Auth.Credentials
import HBS2.Net.IP.Addr
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Net.PeerLocator
import HBS2.Net.Proto.PeerMeta
import HBS2.System.Logger.Simple

-- import PeerInfo
import Brains
import PeerConfig

import Prelude hiding (log)
import Data.Foldable (for_)
import Control.Concurrent.Async
-- import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Writer qualified as W
import Crypto.Saltine.Core.Box qualified as Encrypt
import Data.ByteString.Lazy (ByteString)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as L
import Data.Maybe
import Lens.Micro.Platform
import Data.Hashable
import Type.Reflection
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Heap qualified as Heap
import Data.Heap (Heap,Entry(..))
import Data.Time.Clock
import Data.Word
import Data.List qualified as List

import UnliftIO.STM

import Streaming.Prelude qualified as S

data PeerInfo e =
  PeerInfo
  { _peerBurst          :: TVar Int
  , _peerBurstMax       :: TVar (Maybe Int)
  , _peerBurstSet       :: TVar IntSet
  , _peerErrors         :: TVar Int
  , _peerErrorsLast     :: TVar Int
  , _peerErrorsPerSec   :: TVar Int
  , _peerLastWatched    :: TVar TimeSpec
  , _peerDownloaded     :: TVar Int
  , _peerDownloadedLast :: TVar Int
  , _peerPingFailed     :: TVar Int
  , _peerDownloadedBlk  :: TVar Int
  , _peerDownloadFail   :: TVar Int
  , _peerDownloadMiss   :: TVar Int
  , _peerRTTBuffer      :: TVar [Integer] -- ^ Contains a list of the last few round-trip time (RTT) values, measured in nanoseconds.
                                          -- Acts like a circular buffer.
  , _peerHttpApiAddress :: TVar (Either Int (Maybe String))
  , _peerHttpDownloaded :: TVar Int
  , _peerMeta           :: TVar (Maybe PeerMeta)
  }
  deriving stock (Generic,Typeable)

makeLenses 'PeerInfo

newPeerInfo :: MonadIO m => m (PeerInfo e)
newPeerInfo = liftIO do
  _peerBurst          <- newTVarIO defBurst
  _peerBurstMax       <- newTVarIO Nothing
  _peerBurstSet       <- newTVarIO mempty
  _peerErrors         <- newTVarIO 0
  _peerErrorsLast     <- newTVarIO 0
  _peerErrorsPerSec   <- newTVarIO 0
  _peerLastWatched    <- newTVarIO 0
  _peerDownloaded     <- newTVarIO 0
  _peerDownloadedLast <- newTVarIO 0
  _peerPingFailed     <- newTVarIO 0
  _peerDownloadedBlk  <- newTVarIO 0
  _peerDownloadFail   <- newTVarIO 0
  _peerDownloadMiss   <- newTVarIO 0
  _peerRTTBuffer      <- newTVarIO []
                                          -- Acts like a circular buffer.
  _peerHttpApiAddress <- newTVarIO (Left 0)
  _peerHttpDownloaded <- newTVarIO 0
  _peerMeta           <- newTVarIO Nothing
  pure PeerInfo {..}

type instance SessionData e (PeerInfo e) = PeerInfo e

newtype instance SessionKey e  (PeerInfo e) =
  PeerInfoKey (Peer e)

deriving newtype instance Hashable (SessionKey L4Proto (PeerInfo L4Proto))
deriving stock instance Eq (SessionKey L4Proto (PeerInfo L4Proto))

-- FIXME: this?
instance Expires (SessionKey L4Proto (PeerInfo L4Proto)) where
  expiresIn = const (Just defCookieTimeoutSec)



type MyPeer e = ( Eq (Peer e)
                , Hashable (Peer e)
                , Pretty (Peer e)
                , HasPeer e
                , ForSignedBox e
                )

data DownloadReq e

data DownloadAsap e

data instance EventKey e (DownloadReq e) =
  DownloadReqKey
  deriving (Generic,Typeable,Eq)

instance Typeable (DownloadReq e) => Hashable (EventKey e (DownloadReq e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @DownloadReq

newtype instance Event e (DownloadReq e) =
  DownloadReqData (Hash HbSync)
  deriving (Typeable)

instance EventType ( Event e (DownloadReq e) ) where
  isPersistent = True

instance Expires (EventKey e (DownloadReq e)) where
  expiresIn = const Nothing



type DownloadFromPeerStuff e m = ( MyPeer e
                                 , MonadIO m
                                 , ForSignedBox e
                                 , Request e (BlockInfo e) m
                                 , Request e (BlockChunks e) m
                                 , MonadReader (PeerEnv e ) m
                                 , PeerMessaging e
                                 , HasProtocol e (BlockInfo e)
                                 , EventListener e (BlockInfo e) m
                                 , EventListener e (BlockChunks e) m
                                 , Sessions e (BlockChunks e) m
                                 , Sessions e (PeerInfo e) m
                                 , HasStorage m
                                 )


calcBursts :: forall a . Integral a => a -> [a] -> [(a,a)]
calcBursts bu pieces = go seed
  where
    seed = fmap (,1) pieces

    go ( (n1,s1) : (n2,s2) : xs )
      | (s1 + s2) <= bu = go ((n1, s1+s2) : xs)
      | otherwise  = (n1,s1) : go ( (n2,s2) : xs)

    go [x] = [x]
    go [] = []

data BlockDownload =
  BlockDownload
  { _sBlockHash      :: Hash HbSync
  , _sBlockSize      :: Size
  , _sBlockChunkSize :: ChunkSize
  , _sBlockChunks    :: TQueue (ChunkNum, ByteString)
  , _sBlockChunks2   :: TVar (IntMap ByteString)
  }
  deriving stock (Typeable)

makeLenses 'BlockDownload

newBlockDownload :: MonadIO m => Hash HbSync -> m BlockDownload
newBlockDownload h = liftIO do
  BlockDownload h 0 0 <$> newTQueueIO <*> newTVarIO mempty


type instance SessionData e (BlockChunks e) = BlockDownload

newtype instance SessionKey e (BlockChunks e) =
  DownloadSessionKey (Peer e, Cookie e)
  deriving stock (Generic,Typeable)

deriving newtype instance Hashable (SessionKey L4Proto (BlockChunks L4Proto))
deriving stock instance Eq (SessionKey L4Proto (BlockChunks L4Proto))

data BlockState =
  BlockState
  { _bsStart         :: TimeSpec
  , _bsReqSizeTimes  :: TVar Int
  , _bsLastSeen      :: TVar TimeSpec
  , _bsHasSize       :: TVar Bool
  }

makeLenses 'BlockState

data DownloadMonEnv =
  DownloadMonEnv
  { _downloads :: TVar (HashMap HashRef (IO ()))
  }

makeLenses 'DownloadMonEnv

downloadMonNew :: MonadIO m => m DownloadMonEnv
downloadMonNew = DownloadMonEnv <$> newTVarIO mempty

downloadMonAdd :: forall m . MonadIO m
               => DownloadMonEnv
               -> HashRef
               -> IO ()
               -> m ()

downloadMonAdd env h whenDone = do
  atomically $ modifyTVar (view downloads env) (HashMap.insert h whenDone)

data DownloadEnv e =
  DownloadEnv
  { _blockInQ       :: TVar   (HashMap (Hash HbSync) ())
  , _blockPostponed :: TVar   (HashMap (Hash HbSync) () )
  , _blockPostponedTo :: Cache  (Hash HbSync) ()
  , _blockDelayTo   :: TQueue (Hash HbSync)
  , _blockProposed  :: Cache (Hash HbSync, Peer e) ()
  , _downloadMon    :: DownloadMonEnv
  , _downloadBrains :: SomeBrains e
  }

makeLenses 'DownloadEnv


newDownloadEnv :: (MonadIO m, MyPeer e, HasBrains e brains) => brains -> m (DownloadEnv e)
newDownloadEnv brains = liftIO do
  DownloadEnv <$> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just defBlockBanTime)
              <*> newTQueueIO
              <*> Cache.newCache (Just (toTimeSpec (2 :: Timeout 'Seconds)))
              <*> downloadMonNew
              <*> pure (SomeBrains brains)

newtype BlockDownloadM e m a =
  BlockDownloadM { fromBlockDownloadM :: ReaderT (DownloadEnv e) m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader (DownloadEnv e)
                   , MonadTrans
                   )

withDownload :: (MyPeer e, HasPeerLocator e m, MonadIO m) => DownloadEnv e -> BlockDownloadM e m a -> m a
withDownload e m = runReaderT ( fromBlockDownloadM m ) e


isBlockHereCached :: forall e m . ( MyPeer e
                                  , MonadIO m
                                  , HasStorage m
                                  )
            => Hash HbSync -> BlockDownloadM e m Bool

isBlockHereCached h = do
  sto <- lift getStorage
  liftIO $ hasBlock sto h <&> isJust


type DownloadConstr e m = ( MyPeer e
                          , MonadIO m
                          , HasPeerLocator e (BlockDownloadM e m)
                          , HasStorage m -- (BlockDownloadM e m)
                          )

addDownload :: forall e m . ( DownloadConstr e m
                            )
            => Maybe (Hash HbSync)
            -> Hash HbSync
            -> BlockDownloadM e m ()

addDownload mbh h = do

  tinq <- asks (view blockInQ)
  brains <- asks (view downloadBrains)
  here <- isBlockHereCached h

  if here then do
    removeFromWip h
  else do
    maybe1 mbh none $ \hp -> claimBlockCameFrom @e brains hp h
    liftIO $ atomically $ modifyTVar tinq $ HashMap.insert h ()

postponedNum :: forall e  m . (MyPeer e, MonadIO m) => BlockDownloadM e m Int
postponedNum = do
  po <- asks (view blockPostponed)
  liftIO $ readTVarIO po <&> HashMap.size

isPostponed :: forall e  m . (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m Bool
isPostponed h = do
  po <- asks (view blockPostponed) >>= liftIO . readTVarIO
  pure $ HashMap.member  h po


delayLittleBit :: forall e  m . (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m ()
delayLittleBit h = do
  q <- asks (view blockDelayTo)
  liftIO $ atomically $ writeTQueue q h

postponeBlock :: forall e  m . (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m ()
postponeBlock h = do

  brains <- asks (view downloadBrains)
  po <- asks (view blockPostponed)
  tto <- asks (view blockPostponedTo)
  tinq <- asks (view blockInQ)


  liftIO $ do
    postponed <- atomically $ do
      already <- readTVar po <&> HashMap.member h
      unless already do
        modifyTVar tinq $ HashMap.delete h
        modifyTVar po (HashMap.insert h ())
      pure $ not already

    when postponed do
      Cache.insert tto h ()
      onBlockPostponed @e brains h

unpostponeBlock :: forall e m . (DownloadConstr e m) => Hash HbSync -> BlockDownloadM e m ()
unpostponeBlock h = do

  po <- asks (view blockPostponed)
  tto <- asks (view blockPostponedTo)

  liftIO $ do
      atomically $ modifyTVar po (HashMap.delete h)
      Cache.delete tto h

  trace $ "unpostponeBlock" <+> pretty h
  addDownload @e mzero h

removeFromWip :: (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m ()
removeFromWip h = do
  tinq <- asks (view blockInQ)
  liftIO $ atomically $ do
    modifyTVar' tinq (HashMap.delete h)

failedDownload :: forall e m . ( MyPeer e
                               , MonadIO m
                               , HasPeer e
                               , HasPeerLocator e (BlockDownloadM e m)
                               , HasStorage m
                               )
               => Peer e
               -> Hash HbSync
               -> BlockDownloadM e m ()

failedDownload p h = do
  trace $ "failedDownload" <+> pretty p <+> pretty h
  addDownload mzero h
  -- FIXME: brains-download-fail

type ForGossip e p m =
  ( MonadIO m
  , MyPeer e
  , HasPeerLocator e m
  , HasProtocol e p
  , Request e p m
  , Sessions e (KnownPeer e) m
  )

broadCastMessage :: forall e p m . ( ForGossip e p m )
                   => p -> m ()

broadCastMessage msg = do
  -- TODO: broadcast-reflog-update
  trace "broadCastMessage"
  forKnownPeers $ \pip _ -> do
    trace $ "send msg to peer" <+> pretty pip
    request @e pip msg

forKnownPeers :: forall e  m . ( MonadIO m
                               , HasPeerLocator e m
                               , Sessions e (KnownPeer e) m
                               , HasPeer e
                               )
               =>  ( Peer e -> PeerData e -> m () ) -> m ()
forKnownPeers m = do
  pl <- getPeerLocator @e
  pips <- knownPeers @e pl
  for_ pips $ \p -> do
    mpde <- find (KnownPeerKey p) id
    maybe1 mpde (pure ()) (m p)

getKnownPeers :: forall e  m . ( MonadIO m
                               , HasPeerLocator e m
                               , Sessions e (KnownPeer e) m
                               , HasPeer e
                               )
               =>  m [Peer e]

getKnownPeers  = do
  pl <- getPeerLocator @e
  pips <- knownPeers @e pl
  r <- forM pips $ \p -> do
    pd' <- find (KnownPeerKey p) id
    maybe1 pd' (pure mempty) (const $ pure [p])
  pure $ mconcat r

mkPeerMeta :: PeerConfig -> PeerEnv e -> AnnMetaData
mkPeerMeta conf penv = do
    let mHttpPort :: Maybe Integer
        mHttpPort = cfgValue @PeerHttpPortKey conf <&> fromIntegral
    let mTcpPort :: Maybe Word16
        mTcpPort =
          (
          fmap (\(L4Address _ (IPAddrPort (_, p))) -> p)
            . fromStringMay @(PeerAddr L4Proto)
          )
          =<< cfgValue @PeerListenTCPKey conf
    annMetaFromPeerMeta . PeerMeta $ W.execWriter do
      mHttpPort `forM` \p -> elem "http-port" (TE.encodeUtf8 . Text.pack . show $ p)
      mTcpPort `forM` \p -> elem "listen-tcp" (TE.encodeUtf8 . Text.pack . show $ p)

  where
    elem k = W.tell . L.singleton . (k ,)


-- FIXME: slow-deep-scan-exception-seems-not-working
checkDownloaded :: forall m . (MonadIO m, HasStorage m) => HashRef -> m Bool
checkDownloaded hr = do
  sto <- getStorage
  let readBlock h = liftIO $ getBlock sto h

  result <- S.toList_ $
              deepScan ScanDeep (const $ S.yield Nothing) (fromHashRef hr) readBlock $ \ha -> do
                unless (fromHashRef hr == ha) do
                  here <- liftIO $ hasBlock sto ha
                  S.yield here

  pure $ maybe False (not . List.null)  $ sequence result

data Polling =
  Polling
  { waitBefore   :: NominalDiffTime
  , waitOnEmpty  :: NominalDiffTime
  }

polling :: forall a m . (MonadIO m, Hashable a)
        => Polling
        -> m [(a, NominalDiffTime)]
        -> (a -> m ())
        -> m ()

polling o listEntries action = do

  -- FIXME: might-be-concurrent

  pause (TimeoutNDT (waitBefore o))

  now0 <- getTimeCoarse
  refs0 <- listEntries <&> fmap (set _2 now0) <&> HashMap.fromList

  fix (\next mon -> do
    now <- getTimeCoarse
    refs <- listEntries <&> HashMap.fromList
    let mon' = mon `HashMap.union`
                 HashMap.fromList [ (e, now + toTimeSpec (TimeoutNDT t))
                                  | (e, t) <- HashMap.toList refs
                                  ]

    let q = Heap.fromList [ Entry t e
                          | (e, t) <- HashMap.toList mon'
                          ]

    case Heap.uncons q of
      Just (Entry t r, _) | t <= now -> do
        action r
        next (HashMap.delete r mon')

      Just (Entry t _, _) | otherwise -> do
        pause (TimeoutTS (t - now))
        next mon'

      Nothing -> do
        pause (TimeoutNDT (waitOnEmpty o))
        next mon'

    ) refs0


instance (ForGossip e p (PeerM e IO)) => HasGossip e p (PeerM e IO) where
  gossip msg = do
    broadCastMessage msg

instance (ForGossip e p (ResponseM e m), HasGossip e p m) => HasGossip e p (ResponseM e m) where
  gossip msg = do
    that <- thatPeer (Proxy @p)
    forKnownPeers $ \pip _ -> do
      unless (that == pip) do
        request @e pip msg


simpleBlockAnnounce :: forall e m  . ( Monad m
                                     , HasPeerNonce e m
                                     )
                    => Integer
                    -> Hash HbSync
                    -> m (BlockAnnounce e)

simpleBlockAnnounce size h = do
    no <- peerNonce @e
    let annInfo = BlockAnnounceInfo 0 NoBlockInfoMeta size h
    pure $ BlockAnnounce @e no annInfo

data TRACE1

instance HasLogLevel TRACE1 where
  type instance LogLevel TRACE1 = 101

trace1 :: (MonadIO m, ToLogStr a) => a -> m ()
trace1 = log @TRACE1



