module TestSimpleStorage where

import HBS2.OrDie
import HBS2.Hash
import HBS2.Clock
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Simple

import Control.Monad.Except
import Control.Monad
import Data.Traversable
import Data.Foldable
import Control.Concurrent.Async
import Control.Concurrent
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Prettyprinter
import System.Directory
import System.FilePath.Posix
import System.IO.Temp
import Test.QuickCheck
import System.TimeIt

import Test.Tasty.HUnit



-- CASE:
-- Current result:
-- *** Exception: thread blocked indefinitely in an STM transaction
--
-- Expected result:  survives this situation with honor
testSimpleStorageErrors :: IO ()
testSimpleStorageErrors = do

  withSystemTempDirectory "simpleStorageTest" $ \dir -> do

    let opts = [ StoragePrefix (dir </> ".storage")
               ]

    storage <- simpleStorageInit opts :: IO (SimpleStorage HbSync)

    r <- runExceptT $ liftIO $ do

            worker <- async  (simpleStorageWorker storage)

            let blocks = storage ^. storageBlocks

            p <- getPermissions blocks

            setPermissions blocks (p { readable = False
                                     , searchable = False
                                     , writable = False
                                     })

            let str = "AAAAA" :: LBS.ByteString
            let strKey = hashObject @HbSync str

            key <- putBlock storage str

            assertBool "nothing written" (isNothing key)

            here <- hasBlock storage strKey <&> isJust

            assertBool "nothing written, again" (not here)

            val <- getBlock storage strKey

            assertBool "nothing read" (isNothing val)

            setPermissions blocks p

            mapM_ cancel [worker]

            snd <$> waitAnyCatch [worker]

    case r of
      Left err -> error "oopsie!"
      _        -> pure ()


testSimpleStorageNoKeys :: IO ()
testSimpleStorageNoKeys = do
  withSystemTempDirectory "simpleStorageTest" $ \dir -> do

    let opts = [ StoragePrefix (dir </> ".storage")
               ]

    storage <- simpleStorageInit opts :: IO (SimpleStorage HbSync)

    worker <- async  (simpleStorageWorker storage)

    link worker

    let pieces = take 1000 $ shrink [0x00 .. 0xFF] :: [[Word8]]

    results' <- forConcurrently pieces $ \p -> do
                  let hash = hashObject @HbSync (LBS.pack p)
                  s <- getBlock storage hash
                  pure (LBS.length <$> s)

    let results = catMaybes results'

    print ("results", length results)

    assertBool "no-results" (null results)

    pause ( 0.05 :: Timeout 'Seconds )

    cancel worker

    pure ()


testSimpleStorageRandomReadWrite :: IO ()
testSimpleStorageRandomReadWrite = do

  withSystemTempDirectory "simpleStorageTest" $ \dir -> do

    let opts = [ StoragePrefix (dir </> ".storage")
               ]

    storage <- simpleStorageInit [StoragePrefix (dir </> ".storage")] :: IO (SimpleStorage HbSync)

    exists <- doesDirectoryExist ( storage ^. storageBlocks )

    assertBool "blocks directory exists" exists

    workers <- replicateM 2 $ async  (simpleStorageWorker storage)

    let pieces = shrink [0x00 .. 0xFF] :: [[Word8]]

    forConcurrently_ (take 1000 pieces) $ \piece -> do
    -- for_ (take 10 pieces) $ \piece -> do

      let str = LBS.pack piece

      key <- putBlock storage str

      -- threadDelay $ 500000
      -- print "ok"

      assertBool "key is Just" (isJust key)

      let hash = fromJust key

      -- print (pretty key)

      s <- getBlock storage hash

      -- print s

      assertBool "data read" (isJust s)

      let result = fromJust s

      assertEqual "written data == read data" str result

      let chuSize = 16

      let chNum =
            let (n,r) = length piece `divMod` chuSize
            in if r == 0 then n else succ n

      chunks' <- forM [0,chuSize ..  (chNum - 1)*chuSize] $ \o -> do
                  getChunk storage hash (fromIntegral o) (fromIntegral chuSize)

      let fromChunks = mconcat $ catMaybes chunks'

      -- print (LBS.length  str, LBS.length fromChunks, chNum)

      assertEqual "bs from chunks == str" str fromChunks

      pure ()

    mapM_ cancel workers


testSimpleStorageRefs :: IO ()
testSimpleStorageRefs  = do
  withSystemTempDirectory "simpleStorageTest" $ \dir -> do

    let opts = [ StoragePrefix (dir </> ".storage")
               ]

    storage <- simpleStorageInit opts :: IO (SimpleStorage HbSync)

    worker <- async  (simpleStorageWorker storage)

    link worker

    let k  = "JOPAKITA" :: LBS.ByteString
    let v  = "PECHENTRESKI" :: LBS.ByteString

    vh <- putBlock storage v `orDie` "cant write"

    updateRef storage k vh

    qqq <- simpleReadLinkRaw storage (hashObject k)

    pechen <- getRef storage k

    assertEqual "kv1" (Just vh) pechen

    non <- getRef storage ("QQQQQ" :: LBS.ByteString)

    assertEqual "kv2" Nothing non

    pure ()

