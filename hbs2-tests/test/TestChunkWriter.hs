module Main where

import HBS2.Prelude
import HBS2.Actors.ChunkWriter
import HBS2.Hash
import HBS2.Clock
import HBS2.Storage
import HBS2.Storage.Simple

import Data.Maybe
import Control.Monad.Except
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as B8
import Data.Fixed
import Data.Functor
import qualified Data.Vector.Unboxed as U
import System.FilePath.Posix
import System.IO.Temp
import System.Random.MWC
import System.Random.Shuffle
import System.TimeIt

import Control.DeepSeq
import Control.Exception (evaluate)

import Data.List qualified as L
import Prettyprinter

main :: IO ()
main = do

  -- let size = 1024*1024*1
  let size = 256*1024
  let chu = 500

  g <- initialize $ U.fromList [0xFAFA, 0xBEBE, 0xC0C0]

  bytes <- B8.pack <$> replicateM size (uniformM g)

  let hash = hashObject bytes

  let psz = calcChunks (fromIntegral size) (fromIntegral chu)

  psz' <- shuffleM psz

  withSystemTempDirectory "cww-test" $ \dir -> do

    let opts = [ StoragePrefix (dir </> ".test-cww")
               ]

    storage <- simpleStorageInit opts :: IO (SimpleStorage HbSync)

    w1 <- replicateM 4 $ async (simpleStorageWorker storage)

    cw <- newChunkWriterIO @HbSync storage (Just (dir </> ".qqq"))

    w2 <- replicateM 8 $ async $ runChunkWriter cw

    let times = 1000

    let info = show $ "writing" <+> pretty (show (realToFrac size / (1024*1024) :: Fixed E2))
                                <+> "mb"
                                <+> pretty times <+> "times"

    timeItNamed info $ do

      failed <- replicateM times $ do


          forConcurrently_ psz' $ \(o,s) -> do
            let t = B8.take s $ B8.drop o bytes
            writeChunk cw 1 hash (fromIntegral o) t

          h2 <- getHash cw 1 hash

          if Just hash /= h2 then do
            pure [1]
          else do
            commitBlock cw 1 hash
            pure mempty

      mapM_ cancel $ w1 <> w2

      print $ "failed" <+> pretty (sum (mconcat failed))



