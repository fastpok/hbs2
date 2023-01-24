{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import HBS2.Actors.ChunkWriter
import HBS2.Actors
import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Messaging.Fake
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockAnnounce
import HBS2.Net.Proto.BlockChunks
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra

import Test.Tasty.HUnit

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

import System.Random.MWC
import qualified Data.Vector.Unboxed as U

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p

data Fake

data BlockDownload =
  BlockDownload
  { _sBlockHash      :: Hash HbSync
  , _sBlockSize      :: Size
  , _sBlockChunkSize :: ChunkSize
  , _sBlockOffset    :: Offset
  , _sBlockWritten   :: Size
  }
  deriving stock (Typeable)

makeLenses 'BlockDownload

newBlockDownload :: Hash HbSync -> BlockDownload
newBlockDownload h = BlockDownload h 0 0 0 0

instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Word8
                               deriving newtype (Hashable,Num,Enum,Real,Integral)
                               deriving stock (Eq,Ord,Show)


instance Pretty (Peer Fake) where
  pretty (FakePeer n) = parens ("peer" <+> pretty n)


instance HasProtocol Fake (BlockInfo Fake) where
  type instance ProtocolId (BlockInfo Fake) = 1
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

-- FIXME: 3 is for debug only!
instance Expires (EventKey Fake (BlockInfo Fake)) where
  expiresIn _  = Just 600

instance Expires (EventKey Fake (BlockChunks Fake)) where
  expiresIn _ = Just 600

instance Expires (EventKey Fake (BlockAnnounce Fake)) where
  expiresIn _ = Nothing

instance HasProtocol Fake (BlockChunks Fake) where
  type instance ProtocolId (BlockChunks Fake) = 2
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance HasProtocol Fake (BlockAnnounce Fake) where
  type instance ProtocolId (BlockAnnounce Fake) = 3
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


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

runTestPeer :: Peer Fake
            -> (SimpleStorage HbSync -> ChunkWriter HbSync IO  -> IO ())
            -> IO ()

runTestPeer p zu = do

  dir <- liftIO $ canonicalizePath ( ".peers" </> show (fromIntegral p :: Int))
  let chDir = dir </> "tmp-chunks"
  liftIO $ createDirectoryIfMissing True dir

  let opts = [ StoragePrefix dir
             ]

  stor <- simpleStorageInit opts
  cww  <- newChunkWriterIO  stor (Just chDir)

  sw <- liftIO $ replicateM 4  $ async $ simpleStorageWorker stor
  cw <- liftIO $ replicateM 8 $ async $ runChunkWriter cww

  zu stor cww

  simpleStorageStop stor
  stopChunkWriter cww

  mapM_ cancel $ sw <> cw


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

data DownloadTask e = DownloadTask (Hash HbSync) (Maybe (Peer e, Integer))

blockDownloadLoop :: forall e  m . ( m ~ PeerM e IO
                                 -- , e ~ Fake
                                 , Serialise (Encoded e)
                                 , MonadIO m
                                 , Request e (BlockInfo e) m
                                 , Request e (BlockAnnounce e) m
                                 , HasProtocol e (BlockInfo e)
                                 , HasProtocol e (BlockAnnounce e)
                                 , EventListener e (BlockInfo e) m
                                 , EventListener e (BlockChunks e) m
                                 , EventListener e (BlockAnnounce e) m
                                 -- , EventEmitter e (BlockChunks e) m
                                 -- , EventEmitter e (BlockInfo e) m
                                 , Sessions e (BlockInfo e) m
                                 , Sessions e (BlockChunks e) m
                                 , HasStorage m
                                 , Num (Peer e)
                                 , Pretty (Peer e)
                                 , Block ByteString ~ ByteString
                                 -- , Key HbSync ~ Hash HbSync
                                 )
                  =>  ChunkWriter HbSync IO -> m ()
blockDownloadLoop cw = do

  stor <- getStorage


  let blks = [ "GTtQp6QjK7G9Sh5Aq4koGSpMX398WRWn3DV28NUAYARg"
             ]

  blq  <- liftIO $ Q.newTBQueueIO defBlockDownloadQ
  for_ blks $ \b -> liftIO $ atomically $ Q.writeTBQueue blq (DownloadTask b Nothing)

  subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p ann) -> do
    let h = view biHash ann
    let s = view biSize ann

    debug $ "BLOCK ANNOUNCE!" <+> pretty p
                              <+> pretty h
                              <+> pretty (view biSize ann)

    liftIO $ atomically $ Q.writeTBQueue blq (DownloadTask h (Just (p,s)))

  fix \next -> do

    -- debug $ "WIP:" <+> pretty wip

    job <- liftIO $ atomically $ Q.readTBQueue blq
    wip <- liftIO $ blocksInProcess cw


    if wip > 200 then do
      pause ( 1 :: Timeout 'Seconds )
    else do
      case job of
        DownloadTask hx (Just (p,s)) -> do
          initDownload True blq p hx s

        DownloadTask h Nothing -> do

          peers <- getPeerLocator @e >>= knownPeers @e

          for_ peers $ \peer -> do
            subscribe @e (BlockSizeEventKey h) $ \(BlockSizeEvent (p,hx,s)) -> do
              debug $ "got block size for" <+> pretty h
              liftIO $ atomically $ Q.writeTBQueue blq (DownloadTask hx (Just (p,s)))

            debug $ "requesting size for" <+> pretty h
            request @e peer (GetBlockSize @e h)

    next

  where

    initDownload anyway q p h s = do

      debug $ "initDownload" <+> pretty h <+> pretty p <+> pretty s

      sto <- getStorage
      here <- liftIO $ hasBlock sto h <&> isJust

      if | not here -> do

          coo <- genCookie (p,h)
          let key = DownloadSessionKey (p, coo)
          let chusz = defChunkSize
          let new =   set sBlockChunkSize chusz
                    . set sBlockSize (fromIntegral s)
                      $ newBlockDownload h

          update @e new key id

          subscribe @e (BlockChunksEventKey h) $ \(BlockReady _) -> do
            processBlock q h

          request @e p (BlockChunks @e coo (BlockGetAllChunks @e h chusz)) -- FIXME: nicer construction

         | anyway -> processBlock q h

         | otherwise -> do
            debug $ "already got " <+> pretty h <+> " so relax"
            pure ()

    processBlock q h = do

      env <- ask
      pip <- asks (view envDeferred)
      -- debug "process block!"
      liftIO $ addJob pip $ withPeerM env $ do

        sto <- getStorage
        liftIO $ async $ debug $ "GOT BLOCK!" <+> pretty h
        bt <- liftIO $ getBlock sto h <&> fmap (tryDetect h)
        -- debug $ pretty (show bt)

        case bt of
          Nothing -> pure ()

          Just (AnnRef{}) -> do
            pure ()

          Just (Merkle{}) -> liftIO do
            debug $ "GOT MERKLE. requesting nodes/leaves" <+> pretty h
            walkMerkle h (getBlock sto)  $ \(hr :: [HashRef]) -> do

              for_ hr $ \(HashRef blk) -> do

                here <- liftIO $ hasBlock sto blk <&> isJust

                if here then do
                  debug $ "block" <+> pretty blk <+> "is already here"
                  pure () -- we don't need to recurse, cause walkMerkle is recursing for us

                else do
                  -- if block is missed, then
                  -- block to download q
                  liftIO $ atomically $ Q.writeTBQueue q (DownloadTask blk Nothing)

          Just (Blob{}) -> do
            pure ()


-- NOTE: this is an adapter for a ResponseM monad
--       because response is working in ResponseM monad (ha!)
--       So don't be confused with types
--
mkAdapter :: forall e m . ( m ~  PeerM e IO
                          , HasProtocol e (BlockChunks e)
                          , Hashable (SessionKey e (BlockChunks e))
                          , Sessions e (BlockChunks e) (ResponseM e m)
                          , EventEmitter e (BlockChunks e) m
                          , Pretty (Peer e)
                          , Block ByteString ~ ByteString
                          )
          => ChunkWriter HbSync IO -> m (BlockChunksI e (ResponseM e m ))
mkAdapter cww = do
  storage <- getStorage
  pure $
    BlockChunksI
    { blkSize     = liftIO . hasBlock storage
    , blkChunk    = \h o s -> liftIO (getChunk storage h o s)
    , blkGetHash  = \c -> find (DownloadSessionKey @e c) (view sBlockHash)

    -- КАК ТОЛЬКО ПРИНЯЛИ ВСЕ ЧАНКИ (ПРИШЁЛ ПОСЛЕДНИЙ ЧАНК):
    --   СЧИТАЕМ ХЭШ ТОГО, ЧТО ПОЛУЧИЛОСЬ
    --   ЕСЛИ ПОЛУЧИЛОСЬ ХОРОШО --- ТО:
    --     ПЕРЕЗАПИСЫВАЕМ БЛОК В СТОРЕЙДЖ
    --     ГОВОРИМ ОЖИДАЮЩЕЙ СТОРОНЕ, ЧТО БЛОК ПРИНЯТ?
    --     УДАЛЯЕМ КУКУ?
    , blkAcceptChunk = \(c,p,h,n,bs) -> void $ runMaybeT $ do

        let cKey = DownloadSessionKey (p,c)

        -- check if there is a session
        -- FIXME:
        -- TODO: log situation when no session

        ddd <- lift $ find cKey id

        when (isNothing ddd) $ do
          debug "SESSION NOT FOUND!"


        dwnld <- MaybeT $ find cKey id

        -- dwnld <- maybe1 dwnld' (debug "AAAA") $ pure

        -- debug "session found!"

        let bslen = fromIntegral $ B8.length bs

        let mbChSize = view sBlockChunkSize dwnld
        let mbSize   = view sBlockSize dwnld

        let offset0 = fromIntegral n * fromIntegral mbChSize :: Offset

        liftIO $ do
          writeChunk cww cKey h offset0 bs

        let written = view sBlockWritten dwnld + bslen
        let maxOff  = max offset0 (view sBlockOffset dwnld)

        lift $ update dwnld cKey ( over sBlockOffset (max maxOff)
                                 . over sBlockWritten (+ bslen)
                                 )


        -- debug $ "gotShit"     <+> pretty (B8.length bs) <+> pretty (writtenLast) <+> pretty (wr
        -- debug $ "writtenLast" <+> pretty writtenLast

        -- теперь, что бы нас дидосить -- можно
        -- после 2/3 пакетов посылать пакеты из этих двух третей
        -- или вообще мусор, а мы тут будем считать хэши.
        --
        -- кстати, если посылать мусор - то мы еще и на диск
        -- его будем бесконечно писать.
        --
        -- допустим, пришло 4 события.  в обработчике каждого ---
        -- мы не знаем, что там еще 3 события.
        --
        -- таким образом, ни про какое событие нельзя понять,
        -- что оно последнее, если у нас 4 потока.
        --
        -- как же нам узнать, что пришёл последний блок и можно
        -- его коммитить?
        --
        --
        -- вариант N1. повесить "добивающий монитор".
        -- Здесь только апдейтить счётчики, а вот монитор будет уже
        -- смотреть что как и коммитить.
        --
        -- Монитор может быть протухающим.
        -- Как это сделать?

        -- this is updating concurrently,
        -- so collect last data from all posible threads
        writtenLast <- MaybeT $ find cKey (view sBlockWritten)
        maxOffLast  <- MaybeT $ find cKey (view sBlockOffset)

        let mbDone = (maxOffLast + fromIntegral mbChSize) > fromIntegral mbSize
                   && writtenLast >= ( (mbSize * 2) `div` 3 )

        debug $ "blkAcceptChunk" <+> pretty (p,c)
                                 <+> pretty maxOffLast
        --                          <+> pretty n
        --                          <+> pretty (B8.length bs)
        --                          <+> pretty
        -- debug $  "written:" <+> pretty written <+> "/" <+> pretty mbSize

        when mbDone $ lift do

          deferred (Proxy @(BlockChunks e)) $ do
            h1 <- liftIO $ getHash cww cKey h
            -- h1 <- pure  h-- liftIO $ getHash cww cKey h

          -- ПОСЧИТАТЬ ХЭШ
          -- ЕСЛИ СОШЁЛСЯ - ФИНАЛИЗИРОВАТЬ БЛОК
          -- ЕСЛИ НЕ СОШЁЛСЯ - ТО ПОДОЖДАТЬ ЕЩЕ
            when ( h1 == h ) $ do
              liftIO $ commitBlock cww cKey h
              expire cKey
              -- debug "hash matched!"
              emit @e (BlockChunksEventKey h) (BlockReady h)

        when (written > mbSize * defBlockDownloadThreshold) $ do
          debug $ "SESSION DELETED BECAUSE THAT PEER IS JERK:" <+> pretty p
          lift $ expire cKey
          -- ЕСЛИ ТУТ ВИСЕТЬ ДОЛГО, ТО НАС МОЖНО ДИДОСИТЬ,
          -- ПОСЫЛАЯ НЕ ВСЕ БЛОКИ ЧАНКА ИЛИ ПОСЫЛАЯ ОТДЕЛЬНЫЕ
          -- ЧАНКИ ПО МНОГУ РАЗ. А МЫ БУДЕМ ХЭШИ СЧИТАТЬ.
          -- ТАК НЕ ПОЙДЕТ
          -- ТАК ЧТО ТУТ ЖДЁМ, ДОПУСТИМ 2*mbSize  и отваливаемся
    }


main :: IO ()
main = do
  hSetBuffering stderr LineBuffering

  void $ race (pause (600 :: Timeout 'Seconds)) $ do

    fake <- newFakeP2P True <&> Fabriq

    let (p0:ps) = [0..1] :: [Peer Fake]

    -- others
    others <- forM ps $ \p -> async $ runTestPeer p $ \s cw  -> do
                let findBlk = hasBlock s

                -- let size = 1024*1024*1
                let size = 256*1024
                g <- initialize $ U.fromList [fromIntegral p, fromIntegral size]

                bytes <- replicateM size $ uniformM g :: IO [Char]

                let blk = B8.pack bytes

                root <- putAsMerkle s blk

                rootSz <- hasBlock s (fromMerkleHash root)

                debug $ "I'm" <+> pretty p <+> pretty root

                runPeerM (AnyStorage s) fake p $ do
                  adapter <- mkAdapter cw

                  env <- ask
                  liftIO $ async $ withPeerM env $ do
                      maybe1 rootSz (pure ()) $ \rsz -> do
                        pause ( 0.001 :: Timeout 'Seconds )
                        let info = BlockAnnounceInfo 0 NoBlockInfoMeta rsz (fromMerkleHash root)
                        let ann = BlockAnnounce @Fake info
                        request @Fake p0 ann

                  runProto @Fake
                    [ makeResponse (blockSizeProto findBlk dontHandle)
                    , makeResponse (blockChunksProto adapter)
                    , makeResponse blockAnnounceProto
                    ]

    our <- async $ runTestPeer p0 $ \s cw -> do
                let blk = hasBlock s

                void $ async $ forever $ do
                  pause ( 1 :: Timeout 'Seconds )
                  wip <- blocksInProcess cw
                  debug $ "blocks wip:" <+> pretty wip

                runPeerM (AnyStorage s) fake p0 $ do
                  adapter <- mkAdapter cw
                  env <- ask

                  pl <- getPeerLocator @Fake

                  addPeers @Fake pl ps

                  as <- liftIO $ async $ withPeerM env (blockDownloadLoop cw)

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



