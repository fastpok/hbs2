{-# Language TemplateHaskell #-}
{-# Language ScopedTypeVariables #-}
{-# Language UndecidableInstances #-}
module HBS2.Storage.Simple
  ( module HBS2.Storage.Simple
  , StoragePrefix(..)
  , Storage(..)
  , Block
  ) where

import HBS2.Clock
import HBS2.Hash
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Base58

import HBS2.System.Logger.Simple

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO
import System.IO.Error
import System.IO.Temp
import System.AtomicWrite.Writer.LazyByteString qualified as AwLBS
import System.AtomicWrite.Writer.ByteString qualified as AwBS

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)

import System.IO.Posix.MMap ( unsafeMMapFile )

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue qualified as TBQ
import Control.Concurrent.STM.TBMQueue qualified as TBMQ
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Control.Concurrent.STM.TVar qualified as TV



-- NOTE:  random accessing files in a git-like storage
--        causes to file handles exhaust.
--        Therefore, those handles MUST be in something like
--        pool.
--
--        I.e. we're should use something like TBChan to queue
--        operations and wait in getBlock 'till it's completion
--        in order to make the disk access in this fashion safe

type IsSimpleStorageKey h  = ( Eq (Key h)
                             , Hashable (Key h)
                             , IsKey h
                             , Key h ~ Hash h
                             , ToByteString (AsBase58 (Hash h))
                             , FromByteString (AsBase58 (Hash h))
                             )

type instance Block LBS.ByteString = LBS.ByteString


newtype StorageQueueSize = StorageQueueSize { fromQueueSize :: Int }
                           deriving stock   (Data,Show)
                           deriving newtype (Eq,Ord,Enum,Num,Integral,Real)

data SimpleStorage a =
  SimpleStorage
  { _storageDir         :: FilePath
  , _storageOpQ         :: TBMQueue ( IO () )
  , _storageStopWriting :: TVar Bool
  , _storageMMaped      :: TVar (HashMap (Key a) ByteString)
  , _storageMMapedLRU   :: TVar (HashMap (Key a) TimeSpec)
  }

makeLenses ''SimpleStorage

storageBlocks :: SimpleGetter (SimpleStorage h) FilePath
storageBlocks = to f
  where
    f b = _storageDir b </> "blocks"

storageTemp :: SimpleGetter (SimpleStorage h) FilePath
storageTemp = to f
  where
    f b = _storageDir b </> "temp"

storageRefs :: SimpleGetter (SimpleStorage h) FilePath
storageRefs = to f
  where
    f b = _storageDir b </> "refs"

touchForRead :: (MonadIO m, IsSimpleStorageKey h) => SimpleStorage h -> Key h -> m ByteString
touchForRead ss k  = liftIO $ do

  mbs <- readTVarIO mmaped <&> HashMap.lookup k

  case mbs of
    Just bs -> pure bs
    Nothing -> do

      bsmm <- unsafeMMapFile (simpleBlockFileName ss k)

      tick <- getTimeCoarse

      atomically $ do
        modifyTVar' mmaped (HashMap.insert k bsmm)
        modifyTVar' (ss ^. storageMMapedLRU) (HashMap.insert k tick)
        pure bsmm

  where
    mmaped = ss ^. storageMMaped


simpleStorageInit :: forall h m opts . (MonadIO m, Data opts, IsSimpleStorageKey h)
                   => opts -> m (SimpleStorage h)

simpleStorageInit opts = liftIO $ do
  let prefix = uniLastDef "." opts :: StoragePrefix
  let qSize  = uniLastDef 2000 opts :: StorageQueueSize -- FIXME: defaults ?

  stor <- SimpleStorage
                <$> canonicalizePath (fromPrefix prefix)
                <*> TBMQ.newTBMQueueIO (fromIntegral (fromQueueSize qSize))
                <*> TV.newTVarIO False
                <*> TV.newTVarIO mempty
                <*> TV.newTVarIO mempty

  createDirectoryIfMissing True (stor ^. storageBlocks)
  createDirectoryIfMissing True (stor ^. storageTemp)

  let alph = getAlphabet

  for_ alph $ \a -> do
    createDirectoryIfMissing True ( (stor ^. storageBlocks) </> L.singleton a )
    createDirectoryIfMissing True ( (stor ^. storageRefs) </> L.singleton a )

  pure stor

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

simpleAddTask :: SimpleStorage h -> IO () -> IO ()
simpleAddTask s task = do
  atomically $ TBMQ.writeTBMQueue (s ^. storageOpQ) task

simpleStorageStop :: SimpleStorage h -> IO ()
simpleStorageStop ss = do
  atomically $ TV.writeTVar ( ss ^. storageStopWriting ) True
  fix \next -> do
    mt <- atomically $ TBMQ.isEmptyTBMQueue ( ss ^. storageOpQ )
    if mt then do
      atomically $ TBMQ.closeTBMQueue ( ss ^. storageOpQ )
      pure ()
    else
      pause ( 0.01 :: Timeout 'Seconds ) >> next

simpleStorageWorker :: IsSimpleStorageKey h => SimpleStorage h -> IO ()
simpleStorageWorker ss = do

  ops <- async $ fix \next -> do
    s <- atomically $ do TBMQ.readTBMQueue ( ss ^. storageOpQ )
    case s of
      Nothing -> pure ()
      Just a  -> a >> next

  killer <- async $ forever $ do
    pause ( 30 :: Timeout 'Seconds ) -- FIXME: setting
    simpleAddTask ss $ do

      atomically $ do

        alive  <- readTVar ( ss ^. storageMMapedLRU )
        mmaped <- readTVar ( ss ^. storageMMaped )

        let survived = mmaped `HashMap.intersection` alive

        writeTVar ( ss ^. storageMMaped ) survived

  killerLRU <- async $ forever $ do
    pause ( 10 :: Timeout 'Seconds ) -- FIXME: setting
    atomically $ writeTVar ( ss ^. storageMMapedLRU ) mempty

  (_, e) <- waitAnyCatchCancel [ops,killer, killerLRU]

  pure ()

simpleBlockFileName :: Pretty (Hash h) => SimpleStorage h -> Hash h -> FilePath
simpleBlockFileName ss h = path
  where
    (pref,suf) = splitAt 1 (show (pretty h))
    path = view storageBlocks ss </> pref </> suf

simpleRefFileName :: Pretty (Hash h) => SimpleStorage h -> Hash h -> FilePath
simpleRefFileName ss h = path
  where
    (pref,suf) = splitAt 1 (show (pretty h))
    path = view storageRefs ss </> pref </> suf


-- NOTE: reads a whole file into memory!
--       if file size is too big --- it will
--       cause consequences!
--
--       However, we can not hold file
--       handles in lazy bytestrings, because
--       here maybe too many open files
--
--       So, the block MUST be small
--
simpleGetBlockLazy ::  (IsKey h, Pretty (Key h))
                    => SimpleStorage h
                    -> Hash h
                    -> IO (Maybe LBS.ByteString)

simpleGetBlockLazy s key = do
  resQ <- TBMQ.newTBMQueueIO 1 :: IO (TBMQueue (Maybe LBS.ByteString))
  let fn = simpleBlockFileName s key
  let action = do

        r <- tryJust (guard . isDoesNotExistError)
                     (BS.readFile fn <&> LBS.fromStrict)

        result <- case r of
                    Right bytes -> pure (Just bytes)
                    Left _      -> pure Nothing

        void $ atomically $ TBMQ.writeTBMQueue resQ result

  let onFail (_ :: IOError)=  void $ atomically $ TBMQ.writeTBMQueue resQ Nothing

  simpleAddTask s (catch action onFail)

  atomically $ TBMQ.readTBMQueue resQ >>= maybe (pure Nothing) pure

simpleGetChunkLazy :: IsSimpleStorageKey h
                   => SimpleStorage h
                   -> Hash h
                   -> Offset
                   -> Size
                   -> IO (Maybe LBS.ByteString)

simpleGetChunkLazy s key off size = do
  resQ <- TBMQ.newTBMQueueIO 1 :: IO (TBMQueue (Maybe LBS.ByteString))
  let action = do
       let fn = simpleBlockFileName s key

       bs <- (Just <$> touchForRead s key) `catchAny` \_ -> do
                  err $ "simpleGetChunkLazy" <+> pretty key <+> pretty off <+> pretty size
                  pure Nothing

       let result = BS.take (fromIntegral size) . BS.drop (fromIntegral off) <$> bs

       void $ atomically $ TBMQ.writeTBMQueue resQ (LBS.fromStrict <$> result)

  let onFail (_ :: IOError)=  void $ atomically $ TBMQ.writeTBMQueue resQ Nothing

  simpleAddTask s (catch action onFail )

  atomically $ TBMQ.readTBMQueue resQ >>= maybe (pure Nothing) pure

simplePutBlockLazy :: (IsKey h, Hashed h LBS.ByteString)
                   => Bool -- | wait
                   -> SimpleStorage h
                   -> LBS.ByteString
                   -> IO (Maybe (Hash h))

simplePutBlockLazy doWait s lbs = do

  let hash = hashObject lbs
  let fn = simpleBlockFileName s hash
  let fntmp = takeFileName fn
  let tmp = view storageTemp s

  stop <- atomically $ TV.readTVar ( s ^. storageStopWriting )

  size <- simpleBlockExists s hash <&> fromMaybe 0

  if stop then do
    pure Nothing

  else do

    waits <- TBQ.newTBQueueIO 1 :: IO (TBQueue Bool)

    let action | size > 0 = atomically $ TBQ.writeTBQueue waits True
               | otherwise = do
          handle (\(_ :: IOError) -> atomically $ TBQ.writeTBQueue waits False)
                 do
                   withTempFile tmp fntmp $ \tname h -> do
                     BS.hPut h (LBS.toStrict lbs)
                     hClose h
                     renameFile tname fn
                     atomically $ TBQ.writeTBQueue waits True

    simpleAddTask s action

    if doWait then do
      ok <- atomically $ TBQ.readTBQueue waits

      unless ok do
        err $ "simplePutBlockLazy" <+> pretty hash <+> pretty fn

      pure $! if ok then Just hash else Nothing
    else
      pure $ Just hash

-- TODO: should be async as well?
simpleBlockExists :: IsKey h
                  => SimpleStorage h
                  -> Hash h
                  -> IO (Maybe Integer)

simpleBlockExists ss hash = runMaybeT $ do
  let fn = simpleBlockFileName ss hash
  exists <- liftIO $ doesFileExist fn
  unless exists mzero
  liftIO $ getFileSize fn

spawnAndWait :: SimpleStorage h -> IO a -> IO (Maybe a)
spawnAndWait s act = do
  doneQ <- TBMQ.newTBMQueueIO 1
  simpleAddTask s (act >>= \r -> atomically (TBMQ.writeTBMQueue doneQ r))
  atomically $ TBMQ.readTBMQueue doneQ


simpleWriteLinkRaw :: forall h . ( IsSimpleStorageKey h
                                 , Hashed h LBS.ByteString
                                 , ToByteString (AsBase58 (Hash h))
                                 )
                   => SimpleStorage h
                   -> Hash h
                   -> LBS.ByteString
                   -> IO (Maybe (Hash h))

simpleWriteLinkRaw ss h lbs = do
  let fnr = simpleRefFileName ss h

  runMaybeT $ do
    r <- MaybeT $ putBlock ss lbs
    MaybeT $ liftIO $ spawnAndWait ss $ do
      AwBS.atomicWriteFile fnr (toByteString (AsBase58 r))
        `catchAny` \_ -> do
          err $ "simpleWriteLinkRaw" <+> pretty h <+> pretty fnr

      pure h

simpleWriteLinkRawRef :: forall h . ( IsSimpleStorageKey h
                                 , Hashed h LBS.ByteString
                                 , ToByteString (AsBase58 (Hash h))
                                 )
                      => SimpleStorage h
                      -> Hash h
                      -> Hash h
                      -> IO ()

simpleWriteLinkRawRef ss h ref = do
  let fnr = simpleRefFileName ss h
  void $ spawnAndWait ss $ do
    AwBS.atomicWriteFile fnr (toByteString (AsBase58 ref))
      `catchAny` \_ -> do
        err $ "simpleWriteLinkRawRef" <+> pretty h <+> pretty ref <+> pretty fnr

simpleReadLinkRaw :: IsKey h
                  => SimpleStorage h
                  -> Hash h
                  -> IO (Maybe LBS.ByteString)

simpleReadLinkRaw ss hash = do
  let fn = simpleRefFileName ss hash
  rs <- spawnAndWait ss $ do
          -- FIXME: log-this-situation
          (Just <$> LBS.readFile fn) `catchAny` \_ -> do
            err $ "simpleReadLinkRaw" <+> pretty hash <+> pretty fn
            pure Nothing

  pure $ fromMaybe Nothing rs


simpleReadLinkVal :: ( IsKey h
                     , IsSimpleStorageKey h
                     , Hashed h LBS.ByteString
                     , FromByteString (AsBase58 (Hash h))
                     )
                  => SimpleStorage h
                  -> Hash h
                  -> IO (Maybe LBS.ByteString)

simpleReadLinkVal ss hash = do
  let fn = simpleRefFileName ss hash
  rs <- spawnAndWait ss $ do
        (Just <$> BS.readFile fn) `catchAny` \_ -> do
          err $ "simpleReadLinkVal" <+> pretty hash <+> pretty fn
          pure Nothing

  runMaybeT do
      MaybeT . getBlock ss . unAsBase58 =<< MaybeT (pure (fromByteString =<< join rs))

instance ( MonadIO m, IsKey hash
         , Hashed hash LBS.ByteString
         , Key hash ~ Hash hash
         , IsSimpleStorageKey hash
         , Block LBS.ByteString ~ LBS.ByteString
         )
  => Storage (SimpleStorage hash) hash LBS.ByteString m where

  putBlock s lbs = liftIO $ simplePutBlockLazy True s lbs

  enqueueBlock s lbs = liftIO $ simplePutBlockLazy False s  lbs

  getBlock s key = liftIO $ simpleGetBlockLazy s key

  getChunk s k off size = liftIO $ simpleGetChunkLazy s k off size

  hasBlock s k = liftIO $ simpleBlockExists s k

  updateRef ss ref v = do
    let refHash = hashObject @hash ref
    -- liftIO $ print $ "updateRef:" <+> pretty refHash
    void $ liftIO $ simpleWriteLinkRawRef ss refHash v

  getRef ss ref = do
    let refHash = hashObject @hash ref
    runMaybeT do
      bs <- MaybeT $ liftIO $ simpleReadLinkRaw ss refHash
      let bss = LBS.toStrict bs
      parsed <- MaybeT $ pure $ fromByteString bss
      pure $ unAsBase58 parsed

  delBlock ss h = do
    let fn = simpleBlockFileName ss h
    void $ liftIO $ spawnAndWait ss do
      exists <- doesFileExist fn
      when exists (removeFile fn)

  delRef ss ref = do
    let refHash = hashObject @hash ref
    let fn = simpleRefFileName ss refHash
    void $ liftIO $ spawnAndWait ss $ do
      here <- doesFileExist fn
      when here (removeFile fn)

