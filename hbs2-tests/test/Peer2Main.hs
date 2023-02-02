{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Actors
import HBS2.Actors.ChunkWriter
import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Defaults
import HBS2.Events
import HBS2.Merkle
import HBS2.Net.Messaging.UDP
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockAnnounce
import HBS2.Net.Proto.BlockChunks
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Sessions
import HBS2.OrDie
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Simple

import Test.Tasty.HUnit

import System.Random.Shuffle
import Codec.Serialise hiding (encode,decode)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue qualified as Q
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.Default
import Data.Foldable hiding (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Prettyprinter hiding (pipe)
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import Data.Hashable
import Type.Reflection
import Data.Fixed
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.Split (chunksOf)

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet

import Data.Dynamic

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p


calcBursts :: forall a . Integral a => a -> [a] -> [(a,a)]
calcBursts bu pieces = go seed
  where
    seed = fmap (,1) pieces

    go ( (n1,s1) : (n2,s2) : xs )
      | (s1 + s2) <= bu = go ((n1, s1+s2) : xs)
      | otherwise  = (n1,s1) : go ( (n2,s2) : xs)

    go [x] = [x]
    go [] = []

type Fake = UDP

newtype PeerInfo e =
  PeerInfo
  { _peerBurst :: TVar Int
  }
  deriving stock (Generic,Typeable)

makeLenses 'PeerInfo


newPeerInfo :: MonadIO m => m (PeerInfo e)
newPeerInfo = liftIO do
  PeerInfo <$> newTVarIO defBurst


type instance SessionData e (PeerInfo e) = PeerInfo e

newtype instance SessionKey e  (PeerInfo e) =
  PeerInfoKey (Peer e)

deriving newtype instance Hashable (SessionKey Fake (PeerInfo Fake))
deriving stock instance Eq (SessionKey Fake (PeerInfo Fake))

instance Expires (SessionKey Fake (PeerInfo Fake)) where
  expiresIn = const (Just 600)

data BlockDownload =
  BlockDownload
  { _sBlockHash      :: Hash HbSync
  , _sBlockSize      :: Size
  , _sBlockChunkSize :: ChunkSize
  , _sBlockChunks    :: TQueue (ChunkNum, ByteString)
  }
  deriving stock (Typeable)

makeLenses 'BlockDownload

newBlockDownload :: MonadIO m => Hash HbSync -> m BlockDownload
newBlockDownload h = do
  BlockDownload h 0 0 <$> liftIO newTQueueIO


type instance SessionData e (BlockInfo e) = BlockSizeSession e
type instance SessionData e (BlockChunks e) = BlockDownload

newtype instance SessionKey e (BlockChunks e) =
  DownloadSessionKey (Peer e, Cookie e)
  deriving stock (Generic,Typeable)

newtype BlockSizeSession e =
  BlockSizeSession
  { _bsBlockSizes :: Map (Peer e) Size
  }

makeLenses 'BlockSizeSession

instance Ord (Peer e) => Default (BlockSizeSession e) where
  def = BlockSizeSession mempty

deriving stock instance Show (BlockSizeSession Fake)

deriving newtype instance Hashable (SessionKey Fake (BlockChunks Fake))
deriving stock instance Eq (SessionKey Fake (BlockChunks Fake))

runTestPeer :: (Key HbSync ~ Hash HbSync, Storage (SimpleStorage HbSync) HbSync ByteString (ResponseM Fake (PeerM Fake IO)))
            => MessagingUDP
            -> Peer Fake
            -> (SimpleStorage HbSync -> ChunkWriter HbSync IO  -> IO ())
            -> IO ()

runTestPeer mess p zu = do

  dir <- liftIO $ canonicalizePath ( ".peers" </> show (pretty (AsFileName p)))
  let chDir = dir </> "tmp-chunks"
  liftIO $ createDirectoryIfMissing True dir

  let opts = [ StoragePrefix dir
             ]

  udp  <- async $ runMessagingUDP mess
  stor <- simpleStorageInit opts
  cww  <- newChunkWriterIO  stor (Just chDir)

  sw <- liftIO $ replicateM 8  $ async $ simpleStorageWorker stor
  cw <- liftIO $ replicateM 8 $ async $ runChunkWriter cww

  zu stor cww

  simpleStorageStop stor
  stopChunkWriter cww

  mapM_ cancel $ sw <> cw <> [udp]


handleBlockInfo :: forall e m . ( MonadIO m
                                , Sessions e (BlockInfo e) m
                                , Default (SessionData e (BlockInfo e))
                                , Ord (Peer e)
                                , Pretty (Peer e)
                                -- , EventEmitter e (BlockSize e) m
                                )

                => (Peer e, Hash HbSync, Maybe Integer)
                -> m ()

handleBlockInfo (p, h, sz') = do
   maybe1 sz' (pure ()) $ \sz -> do
    let bsz = fromIntegral sz
    update @e def (BlockSizeKey h) (over bsBlockSizes (Map.insert p bsz))


data Stats e =
  Stats
  { _blkNum       :: !Int
  , _blkNumLast   :: !Int
  , _timeLast     :: !TimeSpec
  }
  deriving stock (Typeable,Generic)

makeLenses 'Stats

instance Default (Stats e) where
  def = Stats 0 0 0

newStatsIO :: MonadIO m => m (Stats e)
newStatsIO = pure $ Stats 0 0 0

type instance SessionData e (Stats e) = Stats e

instance Serialise TimeSpec
instance Serialise (Stats e)

data instance SessionKey e (Stats e) = StatsKey
  deriving stock (Typeable,Eq)

instance Typeable (SessionKey e (Stats e)) => Hashable (SessionKey e (Stats e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @(SessionKey e (Stats e))


newtype Speed = Speed (Fixed E1)
                deriving newtype (Ord, Eq, Num, Real, Fractional, Show)

instance Pretty Speed where
  pretty (Speed n) = pretty (show n)


updateStats :: forall e m . (MonadIO m, Sessions e (Stats e) m)
            => Bool -> Int -> m (Stats e)

updateStats updTime blknum = do
  de <- newStatsIO
  stats <- fetch @e True de StatsKey id

  t <- if updTime then do
            liftIO $ getTime Monotonic
         else
            pure (view timeLast stats)

  let blkNumNew = view blkNum stats + blknum

  let blast = if updTime then
                blkNumNew
              else
                view blkNumLast stats

  let newStats = set blkNum blkNumNew
               . set timeLast t
               . set blkNumLast blast
               $ stats

  update @e de StatsKey (const newStats)

  pure newStats

data DownloadEnv e =
  DownloadEnv
  { _downloadQ :: TQueue (Hash HbSync)
  , _peerBusy  :: TVar   (HashMap (Peer e) ())
  }

makeLenses 'DownloadEnv

class (Eq (Peer e), Hashable (Peer e), Pretty (Peer e)) => MyPeer e
instance (Eq (Peer e), Hashable (Peer e), Pretty (Peer e)) => MyPeer e

newDownloadEnv :: (MonadIO m, MyPeer e) => m (DownloadEnv e)
newDownloadEnv = liftIO do
  DownloadEnv <$> newTQueueIO
              <*> newTVarIO mempty

newtype BlockDownloadM e m a =
  BlockDownloadM { fromBlockDownloadM :: ReaderT (DownloadEnv e) m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader (DownloadEnv e)
                   , MonadTrans
                   )

runDownloadM :: (MyPeer e, MonadIO m) => BlockDownloadM e m a -> m a
runDownloadM m = runReaderT ( fromBlockDownloadM m ) =<< newDownloadEnv

withDownload :: (MyPeer e, MonadIO m) => DownloadEnv e -> BlockDownloadM e m a -> m a
withDownload e m = runReaderT ( fromBlockDownloadM m ) e

addDownload :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
addDownload h = do
  q <- asks (view downloadQ)
  liftIO $ atomically $ writeTQueue q h
  -- debug $ "addDownload" <+> pretty h
  -- pause ( 0.25 :: Timeout 'Seconds )

withFreePeer :: (MyPeer e, MonadIO m)
             => Peer e
             -> BlockDownloadM e m ()
             -> BlockDownloadM e m ()
             -> BlockDownloadM e m ()

withFreePeer p n m = do
  busy <- asks (view peerBusy)
  avail <- liftIO $ atomically
                  $ stateTVar busy $
                      \s -> case HashMap.lookup p s of
                              Nothing -> (True, HashMap.insert p () s)
                              Just{}  -> (False, s)
  if not avail
    then n
    else do
      r <- m
      liftIO $ atomically $ modifyTVar busy $ HashMap.delete p
      pure r

getBlockForDownload :: MonadIO m => BlockDownloadM e m (Hash HbSync)
getBlockForDownload = do
  q <- asks (view downloadQ)
  liftIO $ atomically $ readTQueue q

processBlock :: forall e m . ( MonadIO m
                             , HasStorage m
                             , Block ByteString ~ ByteString
                             )
             => Hash HbSync
             -> BlockDownloadM e m ()

processBlock h = do

   sto <- lift getStorage

   bt <- liftIO $ getBlock sto h <&> fmap (tryDetect h)

   case bt of
     Nothing -> addDownload h

     Just (AnnRef{}) -> pure ()

     Just (Merkle{}) -> do
       debug $ "GOT MERKLE. requesting nodes/leaves" <+> pretty h
       walkMerkle h (liftIO . getBlock sto)  $ \(hr :: [HashRef]) -> do

         for_ hr $ \(HashRef blk) -> do

           -- debug $ pretty blk

           here <- liftIO (hasBlock sto blk) <&> isJust

           if here then do
             debug $ "block" <+> pretty blk <+> "is already here"
             pure () -- we don't need to recurse, cause walkMerkle is recursing for us

           else
              addDownload blk

     Just (Blob{}) -> do
       pure ()


downloadFromWithPeer :: forall e m . ( MyPeer e
                                     , MonadIO m
                                     , Request e (BlockInfo e) m
                                     , Request e (BlockChunks e) m
                                     , MonadReader (PeerEnv e ) m
                                     , PeerMessaging e
                                     , HasProtocol e (BlockInfo e)
                                     , EventListener e (BlockInfo e) m
                                     , EventListener e (BlockChunks e) m
                                     , Sessions e (BlockChunks e) m
                                     , Sessions e (PeerInfo e) m
                                     , Block ByteString ~ ByteString
                                     , HasStorage m
                                     )
                     => Peer e
                     -> Hash HbSync
                     -> BlockDownloadM e m ()
downloadFromWithPeer peer h = do


  npi <- newPeerInfo
  pinfo <- lift $ fetch True npi (PeerInfoKey peer) id

  waitSize <- liftIO $ newTBQueueIO 1

  lift $ do
    subscribe @e (BlockSizeEventKey h) $ \(BlockSizeEvent (p1,hx,s)) -> do
      when ( p1 == peer ) $ do
        liftIO $ atomically $ writeTBQueue waitSize s

    request @e peer (GetBlockSize @e h)

  esize <- liftIO $ race ( pause defBlockInfoTimeout  ) do -- FIXME: block size wait time
            atomically $ readTBQueue waitSize

  let mbSize = either (const Nothing) Just esize

  sto <- lift $ getStorage

  case mbSize of
    Nothing -> void $ addDownload h
    Just thisBkSize  -> do

        coo <- genCookie (peer,h)
        let key = DownloadSessionKey (peer, coo)
        let chusz = defChunkSize
        dnwld <- newBlockDownload h
        let chuQ = view sBlockChunks dnwld
        let new =   set sBlockChunkSize chusz
                  . set sBlockSize (fromIntegral thisBkSize)
                    $ dnwld

        lift $ update @e new key id

        let burstSizeT = view peerBurst pinfo

        burstSize <- liftIO $ readTVarIO burstSizeT

        let offsets = calcChunks thisBkSize (fromIntegral chusz)  :: [(Offset, Size)]

        let chunkNums = [ 0 .. pred (length offsets) ]

        let bursts = calcBursts burstSize chunkNums

        -- debug $ "bursts: " <+> pretty bursts

        r  <- liftIO $ newTVarIO (mempty :: IntMap ByteString)
        rq <- liftIO newTQueueIO

        for_ bursts $ liftIO . atomically . writeTQueue rq

        fix \next -> do
          burst <- liftIO $ atomically $ tryReadTQueue rq

          case burst of

            Just (i,chunksN) -> do
              let req = BlockGetChunks h chusz (fromIntegral i) (fromIntegral chunksN)
              lift $ request peer (BlockChunks @e coo req)

              -- TODO: here wait for all requested chunks!
              -- FIXME: it may blocks forever, so must be timeout and retry

              catched <- either id id <$> liftIO ( race ( pause defChunkWaitMax >> pure mempty )
                                                        ( replicateM chunksN
                                                             $ atomically
                                                                $ readTQueue chuQ )

                                                 )
              when (null catched) $ do

                -- nerfing peer burst size.
                -- FIXME: we need a thread that will be reset them again

                newBurst <- liftIO $ atomically
                                   $ stateTVar burstSizeT $ \c -> let v = max 1 (c `div` 2)
                                                                  in (v,v)

                let chuchu = calcBursts newBurst [ i + n | n <- [0 .. chunksN] ]

                debug $ "new burst: " <+> pretty newBurst
                debug $ "missed chunks for request" <+> pretty (i,chunksN)

                for_ chuchu $ liftIO . atomically . writeTQueue rq

              for_ catched $ \(num,bs) -> do
                liftIO $ atomically $ modifyTVar' r (IntMap.insert (fromIntegral num) bs)

              next

            Nothing -> do

              sz <- liftIO $ readTVarIO r <&> IntMap.size

              if sz == length offsets then do
                pieces <- liftIO $ readTVarIO r <&> IntMap.elems
                let block = mconcat pieces
                let h1 = hashObject @HbSync block

                if h1 == h then do
                  -- debug "PROCESS BLOCK"
                  lift $ expire @e key
                  void $ liftIO $ putBlock sto block
                  void $ processBlock h
                else do
                  debug "HASH NOT MATCH"
                  debug "MAYBE THAT PEER IS JERK"

              else do
                debug "RETRY BLOCK DOWNLOADING / ASK FOR MISSED CHUNKS"
                got  <- liftIO $ readTVarIO r <&> IntMap.keysSet
                let need = IntSet.fromList (fmap fromIntegral chunkNums)

                let missed = IntSet.toList $ need `IntSet.difference` got

                -- normally this should not happen
                -- however, let's try do download the tails
                -- by one chunk a time
                for_ missed $ \n -> do
                  liftIO $ atomically $ writeTQueue rq (n,1)


instance HasPeerLocator e m => HasPeerLocator e (BlockDownloadM e m) where
  getPeerLocator = lift getPeerLocator

blockDownloadLoop :: forall e  m . ( m ~ PeerM e IO
                                   ,  MonadIO m
                                   , Request e (BlockInfo e) m
                                   , Request e (BlockAnnounce e) m
                                   , HasProtocol e (BlockInfo e)
                                   , HasProtocol e (BlockAnnounce e)
                                   , HasProtocol e (BlockChunks e)
                                   , EventListener e (BlockInfo e) m
                                   , EventListener e (BlockChunks e) m
                                   , EventListener e (BlockAnnounce e) m
                                   , EventEmitter e (BlockChunks e) m
                                   , Sessions e (BlockInfo e) m
                                   , Sessions e (BlockChunks e) m
                                   , Sessions e (PeerInfo e) m
                                   , PeerSessionKey e (PeerInfo e)
                                   , Typeable (SessionKey e (BlockChunks e))
                                   , Typeable (SessionKey e (BlockInfo e))
                                   , HasStorage m
                                   , Pretty (Peer e)
                                   , Block ByteString ~ ByteString
                                   , PeerMessaging e
                                   )
                  => m ()
blockDownloadLoop = do

  e    <- ask
  stor <- getStorage

  let blks = [ "GTtQp6QjK7G9Sh5Aq4koGSpMX398WRWn3DV28NUAYARg"
             , "ECWYwWXiLgNvCkN1EFpSYqsPcWfnL4bAQADsyZgy1Cbr"
             ]

  pl <- getPeerLocator @e

  -- TODO: peer info loop
  void $ liftIO $ async $ forever $ withPeerM e $ do
    pause @'Seconds 20
    pee <- knownPeers @e pl

    npi <- newPeerInfo

    for_ pee $ \p -> do
      pinfo <- fetch True npi (PeerInfoKey p) id
      burst <- liftIO $ readTVarIO (view peerBurst pinfo)
      debug $ "peer" <+> pretty p <+> "burst: " <+> pretty burst
      pure ()

  runDownloadM @e $ do

    env <- ask

    let again h = do
          debug $ "block fucked: " <+> pretty h
          withPeerM e $ withDownload env (addDownload h)

    mapM_ processBlock blks

    fix \next -> do

      h <- getBlockForDownload

      here <- liftIO $ hasBlock stor h <&> isJust

      unless here do

        void $ runMaybeT $ do
          p  <- MaybeT $ knownPeers @e pl >>= liftIO . shuffleM <&> headMay

          liftIO $ race ( pause defBlockWaitMax >> again h ) do
            withPeerM e $ withDownload env $ do -- NOTE: really crazy shit
              withFreePeer p (addDownload h >> pause (0.1 :: Timeout 'Seconds)) do
                downloadFromWithPeer p h

      next


-- NOTE: this is an adapter for a ResponseM monad
--       because response is working in ResponseM monad (ha!)
--       So don't be confused with types
--
mkAdapter :: forall e m . ( m ~  PeerM e IO
                          , HasProtocol e (BlockChunks e)
                          , Hashable (SessionKey e (BlockChunks e))
                          , Sessions e (BlockChunks e) (ResponseM e m)
                          , Typeable (SessionKey e (BlockChunks e))
                          , Default (SessionData e (Stats e))
                          , EventEmitter e (BlockChunks e) m
                          , Pretty (Peer e)
                          , Block ByteString ~ ByteString
                          )
          => m (BlockChunksI e (ResponseM e m ))
mkAdapter = do
  storage <- getStorage
  pure $
    BlockChunksI
    { blkSize     = liftIO . hasBlock storage
    , blkChunk    = \h o s -> liftIO (getChunk storage h o s)
    , blkGetHash  = \c -> find (DownloadSessionKey @e c) (view sBlockHash)

    , blkAcceptChunk = \(c,p,h,n,bs) -> void $ runMaybeT $ do

        -- debug "AAAA!"

        let cKey = DownloadSessionKey (p,c)

        -- check if there is a session
        -- FIXME:
        -- TODO: log situation when no session

        ddd <- lift $ find cKey id

        when (isNothing ddd) $ do
          debug "SESSION NOT FOUND!"

        dwnld <- MaybeT $ find cKey (view sBlockChunks)

        liftIO $ atomically $ writeTQueue dwnld (n, bs)
    }



main :: IO ()
main = do
  hSetBuffering stderr LineBuffering

  void $ race (pause (600 :: Timeout 'Seconds)) $ do

    -- fake <- newFakeP2P True <&> Fabriq

    udp0 <- newMessagingUDP False (Just "127.0.0.1:10001") `orDie` "Can't start listener on 10001"
    udp1 <- newMessagingUDP False (Just "127.0.0.1:10002") `orDie` "Can't start listener on 10002"

    let (p0:ps) = [getOwnPeer udp0, getOwnPeer udp1]

    -- others
    others <- forM ps $ \p -> async $ runTestPeer udp1 p $ \s cw  -> do
                let findBlk = hasBlock s

                runPeerM (AnyStorage s) (Fabriq udp1) p $ do
                  adapter <- mkAdapter

                  runProto @Fake
                    [ makeResponse (blockSizeProto findBlk dontHandle)
                    , makeResponse (blockChunksProto adapter)
                    , makeResponse blockAnnounceProto
                    ]

    our <- async $ runTestPeer udp0 p0 $ \s cw -> do
                let blk = hasBlock s

                -- void $ async $ forever $ do
                  -- pause ( 1 :: Timeout 'Seconds )
                  -- wip <- blocksInProcess cw
                  -- debug $ "blocks wip:" <+> pretty wip

                runPeerM (AnyStorage s) (Fabriq udp0) p0 $ do
                  adapter <- mkAdapter
                  env <- ask

                  pl <- getPeerLocator @Fake

                  addPeers @Fake pl ps

                  as <- liftIO $ async $ withPeerM env blockDownloadLoop

                  me <- liftIO $ replicateM 1 $ async $ liftIO $ withPeerM env $ do
                    runProto @Fake
                      [ makeResponse (blockSizeProto blk handleBlockInfo)
                      , makeResponse (blockChunksProto adapter)
                      , makeResponse blockAnnounceProto
                      ]

                  liftIO $ mapM_ wait me

                  liftIO $ cancel as

    pause ( 599.9 :: Timeout 'Seconds )

    mapM_ cancel (our:others)

    (_, e) <- waitAnyCatchCancel (our:others)

    debug (pretty $ show e)
    debug "we're done"
    assertBool "success" True
    exitSuccess

  assertBool "failed" False


