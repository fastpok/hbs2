{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import HBS2.Hash
import HBS2.Actors.Peer
import HBS2.Data.Types.Refs
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service
import HBS2.Storage
import HBS2.Storage.Simple (simpleStorageWorker,simpleStorageInit)
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Unix

import HBS2.Peer.RPC.Class
import HBS2.Peer.RPC.Internal.Storage()

import HBS2.OrDie

import HBS2.System.Logger.Simple

import Control.Monad.Reader
import Data.Kind
import System.FilePath
import UnliftIO
import Prettyprinter
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Codec.Serialise

import Test.Tasty.HUnit

instance HasFabriq UNIX (ReaderT (AnyStorage, MessagingUnix) IO) where
  getFabriq = asks (Fabriq . snd)

instance HasOwnPeer UNIX (ReaderT (AnyStorage, MessagingUnix) IO) where
  ownPeer = asks ( msgUnixSelf . snd)

instance Monad m => HasStorage (ReaderT (AnyStorage, MessagingUnix) m) where
  getStorage = asks fst


main :: IO ()
main = do

  setLogging @DEBUG (logPrefix "[debug] ")
  setLogging @INFO (logPrefix "")

  withSystemTempDirectory "storageRpcTest" $ \dir -> do

    let soname = dir </> "rpc.socket"

    let opts = [ StoragePrefix (dir </> ".storage")
               ]

    sto <- simpleStorageInit @HbSync opts

    worker <- async  (simpleStorageWorker sto)
    link worker

    let blk1 = "AAAAA"

    h1 <- putBlock sto blk1 `orDie` "can't write block"

    debug $ "written" <+> pretty h1

    let rk1 = SomeRefKey ("SOMEREFKEY1" :: LBS.ByteString)

    updateRef sto rk1 h1

    rk1val <- getRef sto rk1

    info $ "rk1val:" <+> pretty rk1val

    rk1val1 <- getRef sto (refAlias rk1)

    info $ "rk1val1:" <+> pretty rk1val1

    assertBool "ref-alias-works-1" ( fromJust rk1val == fromJust rk1val1 )

    server <- newMessagingUnix True 1.0 soname
    m1 <- async $ runMessagingUnix server
    link m1

    proto <- async $ flip runReaderT (AnyStorage sto, server) do
      runProto @UNIX
        [ makeResponse (makeServer @StorageAPI)
        ]

    link proto

    withRPC2 @StorageAPI soname $ \caller -> do

      info "does it work?"

      size <- callService @RpcStorageHasBlock caller (HashRef h1) `orDie` "can't read block"

      info $ "got block size: " <+> pretty size

      assertBool "block-size-1" (size == Just (fromIntegral $ LBS.length blk1))

      b <-  callService @RpcStorageGetBlock caller (HashRef h1) `orDie` "can't read block"

      info $ "got block" <+> viaShow b

      assertBool "block-eq-1" ( b == Just blk1 )

      let pechen = "PECHENTERSKI"

      h2 <- callService @RpcStoragePutBlock caller pechen  `orDie` "service error"

      info $ "stored block hash:" <+> pretty h2

      let hh2  = fromJust h2

      blk2 <- callService @RpcStorageGetBlock caller hh2 `orDie` "block lookup failed"

      info $ "stored block value:" <+> viaShow blk2

      assertBool "block-eq-2" (Just pechen == blk2)

      let rk2 = refAlias rk1

      rk2val <- callService @RpcStorageGetRef caller rk2 `orDie` "can't read ref"

      info $ "rk2val:" <+> pretty rk2val

      assertBool "ref-alias-works-2" (fromJust rk2val == HashRef h1)

      callService @RpcStorageUpdateRef caller (rk2, hh2)

      rk3val <- callService @RpcStorageGetRef caller rk2 `orDie` "can't update ref"

      info $ "rk3val" <+> pretty rk3val

      assertBool "ref-alias-update-works-1" (fromJust rk3val == hh2)

      rk4val <- getRef sto rk1

      info $ "rk4val" <+> pretty rk4val

      assertBool "ref-alias-works-2" (fromJust rk4val == fromHashRef hh2)

  setLoggingOff @DEBUG
  setLoggingOff @INFO

