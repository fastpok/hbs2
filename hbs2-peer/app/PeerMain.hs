{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
module Main where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Clock
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types
import HBS2.Net.Auth.Credentials
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging
import HBS2.Net.Messaging.UDP
import HBS2.Net.Messaging.TCP
import HBS2.Net.PeerLocator
import HBS2.Net.Proto as Proto
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Dialog
import HBS2.Net.Proto.EncryptionHandshake
import HBS2.Net.Proto.Event.PeerExpired
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerAnnounce
import HBS2.Net.Proto.PeerExchange
import HBS2.Net.Proto.PeerMeta
import HBS2.Net.Proto.RefLog
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Sessions
import HBS2.OrDie
import HBS2.Storage.Simple
import HBS2.Data.Detect

import HBS2.System.Logger.Simple hiding (info)
import HBS2.System.Logger.Simple qualified as Log

import Brains
import RPC
import PeerTypes
import BlockDownload
import BlockHttpDownload
import DownloadQ
import PeerInfo
import PeerConfig
import Bootstrap
import CheckMetrics
import EncryptionKeys
import RefLog qualified
import RefLog (reflogWorker)
import HttpWorker
import ProxyMessaging
import PeerMain.DialogCliCommand
import PeerMain.PeerDialog
import PeerMeta
import CLI.RefChan
import RefChan

import Codec.Serialise as Serialise
-- import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception as Exception
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS qualified as W
import Crypto.Saltine (sodiumInit)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Cache qualified as Cache
import Data.Function
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text.Encoding qualified as TE
import Data.Text qualified as Text
import Data.Text (Text)
import Data.HashSet qualified as HashSet
import GHC.Stats
import GHC.TypeLits
import Lens.Micro.Platform as Lens
import Network.Socket
import Options.Applicative
import System.Directory
import System.Exit
import System.IO
import System.Mem
import System.Metrics
import System.Posix.Process
import System.Environment
import Text.InterpolatedString.Perl6 (qc)

import UnliftIO.Exception qualified as U
-- import UnliftIO.STM
import UnliftIO.Async as U

import Control.Monad.Trans.Resource
import Streaming.Prelude qualified as S
import Streaming qualified as S

-- TODO: write-workers-to-config
defStorageThreads :: Integral a => a
defStorageThreads = 4

defListenUDP :: String
defListenUDP = "0.0.0.0:7351"

defRpcUDP :: String
defRpcUDP = "localhost:13331"

defLocalMulticast :: String
defLocalMulticast = "239.192.152.145:10153"

data PeerListenKey
data PeerKeyFileKey
data PeerBlackListKey
data PeerWhiteListKey
data PeerStorageKey
data PeerAcceptAnnounceKey
data PeerTraceKey
data PeerTrace1Key
data PeerProxyFetchKey

data AcceptAnnounce = AcceptAnnounceAll
                    | AcceptAnnounceFrom (Set (PubKey 'Sign (Encryption L4Proto)))

instance Pretty AcceptAnnounce where
  pretty = \case
    AcceptAnnounceAll     -> parens ("accept-announce" <+> "*")

    -- FIXME: better-pretty-for-AcceptAnnounceFrom
    AcceptAnnounceFrom xs -> parens ("accept-announce" <+> pretty (fmap AsBase58 (Set.toList xs)))

instance HasCfgKey PeerTraceKey FeatureSwitch where
  key = "trace"

instance HasCfgKey PeerTrace1Key FeatureSwitch where
  key = "trace1"

instance HasCfgKey PeerListenKey (Maybe String) where
  key = "listen"

instance HasCfgKey PeerKeyFileKey (Maybe String) where
  key = "key"

instance HasCfgKey PeerStorageKey (Maybe String) where
  key = "storage"

instance HasCfgKey PeerBlackListKey (Set String) where
  key = "blacklist"

instance HasCfgKey PeerWhiteListKey (Set String) where
  key = "whitelist"

instance HasCfgKey PeerProxyFetchKey (Set String) where
  key = "proxy-fetch-for"

instance HasCfgKey PeerAcceptAnnounceKey AcceptAnnounce where
  key = "accept-block-announce"

instance HasCfgValue PeerAcceptAnnounceKey AcceptAnnounce where
  cfgValue (PeerConfig syn) = fromMaybe (AcceptAnnounceFrom lst) fromAll
    where
      fromAll = headMay [ AcceptAnnounceAll | ListVal @C (Key s [SymbolVal "*"]) <- syn, s == kk ]
      lst = Set.fromList $
                catMaybes [ fromStringMay @(PubKey 'Sign (Encryption L4Proto)) (Text.unpack e)
                          | ListVal @C (Key s [LitStrVal e]) <- syn, s == kk
                          ]
      kk = key @PeerAcceptAnnounceKey @AcceptAnnounce


data PeerOpts =
  PeerOpts
  { _storage       :: Maybe StoragePrefix
  , _listenOn      :: Maybe String
  , _listenRpc     :: Maybe String
  , _peerCredFile  :: Maybe FilePath
  , _peerConfig    :: Maybe FilePath
  , _peerRespawn   :: Maybe Bool
  }
  deriving stock (Data)

makeLenses 'PeerOpts

tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix "[notice] "

main :: IO ()
main = do

  sodiumInit

  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix

  setLoggingOff @TRACE
  setLoggingOff @TRACE1

  withSimpleLogger runCLI

runCLI :: IO ()
runCLI = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "hbs2-peer daemon"
  <> progDesc "serves HBS2 protocol"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "init"      (info pInit (progDesc "creates default config"))
                        <> command "run"       (info pRun  (progDesc "run peer"))
                        <> command "poke"      (info pPoke (progDesc "poke peer by rpc"))
                        <> command "die"       (info pDie (progDesc "die cmd"))
                        <> command "announce"  (info pAnnounce (progDesc "announce block"))
                        <> command "ping"      (info pPing (progDesc "ping another peer"))
                        <> command "fetch"     (info pFetch (progDesc "fetch block"))
                        <> command "reflog"    (info pRefLog (progDesc "reflog commands"))
                        <> command "refchan"   (info pRefChan (progDesc "refchan commands"))
                        <> command "peers"     (info pPeers (progDesc "show known peers"))
                        <> command "pexinfo"   (info pPexInfo (progDesc "show pex"))
                        <> command "log"       (info pLog   (progDesc "set logging level"))

                        <> command "dial"      (info pDialog (progDesc "dialog commands"))
                        )

    confOpt = strOption ( long "config"  <> short 'c' <> help "config" )

    rpcOpt = strOption ( short 'r' <> long "rpc"
                                   <> help "addr:port" )


    common = do
      pref <- optional $ strOption ( short 'p' <> long "prefix"
                                               <> help "storage prefix" )

      l    <- optional $ strOption ( short 'l' <> long "listen"
                                    <> help "addr:port" )

      r    <- optional rpcOpt

      k    <- optional $ strOption ( short 'k' <> long "key"
                                    <> help "peer keys file" )

      c <- optional confOpt

      resp <- optional $ flag' True ( long "respawn" <> short 'R' <> help "respawn process")

      pure $ PeerOpts pref l r k c resp

    pRun = do
      runPeer <$> common

    pRpcCommon = do
      RPCOpt <$> optional confOpt
             <*> optional rpcOpt

    pDie = do
      rpc <- pRpcCommon
      pure $ runRpcCommand rpc DIE

    pPoke = do
      rpc <- pRpcCommon
      pure $ runRpcCommand rpc POKE

    pAnnounce = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "HASH" )
      pure $ runRpcCommand rpc (ANNOUNCE h)

    pFetch = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "HASH" )
      pure $ runRpcCommand rpc (FETCH h)

    pPing = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "ADDR" )
      pure $ runRpcCommand rpc (PING h Nothing)

    pPeers = do
      rpc <- pRpcCommon
      pure $ runRpcCommand rpc PEERS

    pPexInfo = do
      rpc <- pRpcCommon
      pure $ runRpcCommand rpc PEXINFO

    onOff l =
            hsubparser ( command "on" (info (pure (l True) ) (progDesc "on")  ) )
      <|>   hsubparser ( command "off" (info (pure (l False) ) (progDesc "off")  ) )

    pLog = do
      rpc <- pRpcCommon
      setlog <- SETLOG <$> ( hsubparser ( command "trace"  (info (onOff TraceOn) (progDesc "set trace")  ) )
                               <|>
                              hsubparser ( command "debug"  (info (onOff DebugOn) (progDesc "set debug")  ) )
                            )
      pure $ runRpcCommand rpc setlog

    pInit = do
      pref <- optional $ strArgument ( metavar "DIR" )
      pure $ peerConfigInit pref

    pRefLog = hsubparser (  command "send" (info pRefLogSend (progDesc "send reflog transaction" ))
                         <> command "send-raw" (info pRefLogSendRaw (progDesc "send reflog raw transaction" ))
                         <> command "fetch" (info pRefLogFetch (progDesc "fetch reflog from all" ))
                         <> command "get"   (info pRefLogGet (progDesc "get own reflog from all" ))
                         )

    pRefLogSend = do
      rpc <- pRpcCommon
      kr <- strOption (long  "keyring" <> short 'k' <> help "reflog keyring" <> metavar "FILE")
      pure $ do
        setLogging @TRACE tracePrefix
        trace "pRefLogSend"
        s <- BS.readFile kr
        creds <- pure (parseCredentials @(Encryption L4Proto) (AsCredFile s)) `orDie` "bad keyring file"
        bs <- BS.take defChunkSize  <$> BS.hGetContents stdin
        let pubk = view peerSignPk creds
        let privk = view peerSignSk creds
        msg <- makeRefLogUpdate @L4Proto pubk privk bs <&> serialise
        runRpcCommand rpc (REFLOGUPDATE msg)

    pRefLogSendRaw = do
      rpc <- pRpcCommon
      pure $ do
        setLogging @TRACE tracePrefix
        trace "pRefLogSendRaw"
        bs <- LBS.take defChunkSize  <$> LBS.hGetContents stdin
        runRpcCommand rpc (REFLOGUPDATE bs)

    pRefLogFetch = do
      rpc <- pRpcCommon
      ref <- strArgument ( metavar "REFLOG-KEY" )
      pure $ do
        href <- pure (fromStringMay ref) `orDie` "invalid REFLOG-KEY"
        setLogging @TRACE tracePrefix
        trace "pRefLogFetch"
        runRpcCommand rpc (REFLOGFETCH href)

    pRefLogGet = do
      rpc <- pRpcCommon
      ref <- strArgument ( metavar "REFLOG-KEY" )
      pure $ do
        href <- pure (fromStringMay ref) `orDie` "invalid REFLOG-KEY"
        setLogging @TRACE tracePrefix
        runRpcCommand rpc (REFLOGGET href)


myException :: SomeException -> IO ()
myException e = err ( show e )


newtype CredentialsM e s m a =
  CredentialsM { fromCredentials :: ReaderT (PeerCredentials s) m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader (PeerCredentials s)
                   , MonadTrans)

withCredentials :: forall e s m a . (HasOwnPeer e m, Monad m, s ~ Encryption e)
                => PeerCredentials s
                -> CredentialsM e s m a -> m a

withCredentials pc m = runReaderT (fromCredentials m) pc


instance (Monad m, HasTimeLimits e p m, s ~ Encryption e) => HasTimeLimits  e p (CredentialsM e s m) where
  tryLockForPeriod p m = lift $ tryLockForPeriod p m

instance (HasOwnPeer e m) => HasOwnPeer e (CredentialsM e s m) where
  ownPeer = lift ownPeer

instance (Monad m, HasFabriq e m, s ~ Encryption e) => HasFabriq e (CredentialsM e s m)  where
  getFabriq = lift getFabriq

instance (Sessions e p m, s ~ Encryption e) => Sessions e p (CredentialsM e s m)  where
  find k f = lift (find k f)
  fetch i d k f = lift (fetch i d k f)
  update d k f = lift (update d k f)
  expire k = lift (expire  k)

instance (Monad m, HasPeerNonce e m, s ~ Encryption e) => HasPeerNonce e (CredentialsM e s m) where
  peerNonce = lift $ peerNonce @e

instance (Monad m, s ~ Encryption e) => HasCredentials s (CredentialsM e s m) where
  getCredentials = ask

instance (Monad m, s ~ Encryption e) => HasCredentials s (ResponseM e (CredentialsM e s m)) where
  getCredentials = lift getCredentials

instance (Monad m, HasThatPeer e p m, s ~ Encryption e) => HasThatPeer e p (CredentialsM e s m) where
  thatPeer = lift . thatPeer

instance ( EventEmitter e p m
         ) => EventEmitter e p (CredentialsM e s m) where

  emit k d = lift $ emit k d

instance ( Monad m
         , Response e p m
         , s ~ Encryption e
         ) => Response e p (CredentialsM e s m) where

  response = lift . response


respawn :: PeerOpts -> IO ()
respawn opts = case view peerRespawn opts of
  Just True -> do
    let secs = 5
    notice $ "RESPAWNING in" <+> viaShow secs <> "s"
    pause @'Seconds secs
    self <- getExecutablePath
    args <- getArgs
    print (self, args)
    executeFile self False args Nothing

  _ -> exitFailure

runPeer :: forall e s . ( e ~ L4Proto
                        , FromStringMaybe (PeerAddr e)
                        , s ~ Encryption e
                        , HasStorage (PeerM e IO)
                        ) => PeerOpts -> IO ()

runPeer opts = Exception.handle (\e -> myException e
                        >> performGC
                        >> respawn opts
                        ) $ runResourceT do

  metrics <- liftIO newStore

  xdg <- liftIO $ getXdgDirectory XdgData defStorePath <&> fromString

  conf <- peerConfigRead (view peerConfig opts)

  -- let (PeerConfig syn) = conf
  liftIO $ print $ pretty conf

  let listenConf = cfgValue @PeerListenKey conf
  let rpcConf = cfgValue @PeerRpcKey conf
  let keyConf = cfgValue @PeerKeyFileKey conf
  let storConf = cfgValue @PeerStorageKey conf <&> StoragePrefix
  let traceConf = cfgValue @PeerTraceKey conf :: FeatureSwitch
  let trace1Conf = cfgValue @PeerTrace1Key conf :: FeatureSwitch

  let listenSa = view listenOn opts <|> listenConf <|> Just defListenUDP
  let rpcSa = view listenRpc opts <|> rpcConf <|> Just defRpcUDP
  credFile <- pure (view peerCredFile opts <|> keyConf) `orDie` "credentials not set"

  let pref = view storage opts <|> storConf <|> Just xdg

  debug $ "storage prefix:" <+> pretty pref

  debug $ pretty "trace: " <+> pretty (show traceConf)
  debug $ pretty "trace1: " <+> pretty (show trace1Conf)

  when (traceConf == FeatureOn) do
    setLogging @TRACE tracePrefix

  when (trace1Conf == FeatureOn) do
    setLogging @TRACE1 tracePrefix

  let bls = cfgValue @PeerBlackListKey conf :: Set String
  let whs = cfgValue @PeerWhiteListKey conf :: Set String
  let toKeys xs = Set.fromList
                    $ catMaybes [ fromStringMay x | x <- Set.toList xs
                                ]
  let blkeys = toKeys bls
  let wlkeys = toKeys (whs `Set.difference` bls)
  let helpFetchKeys = cfgValue @PeerProxyFetchKey conf & toKeys

  let useHttpDownload = cfgValue @PeerUseHttpDownload conf & (== FeatureOn)

  let accptAnn = cfgValue @PeerAcceptAnnounceKey conf :: AcceptAnnounce

  liftIO $ print $ pretty accptAnn

  -- FIXME: move-peerBanned-somewhere
  let peerBanned p pd = do
        let k = view peerSignKey pd
        let blacklisted = k `Set.member` blkeys
        let whitelisted = Set.null wlkeys || (k `Set.member` wlkeys)
        pure $ blacklisted || not whitelisted

  let acceptAnnounce p pd = do
        case accptAnn of
          AcceptAnnounceAll    -> pure True
          AcceptAnnounceFrom s -> pure $ view peerSignKey pd `Set.member` s

  rpcQ <- liftIO $ newTQueueIO @RPCCommand

  let ps = mempty

  pc' <- liftIO $ LBS.readFile credFile
            <&> parseCredentials @(Encryption e) . AsCredFile
                                                 . LBS.toStrict
                                                 . LBS.take 4096

  pc <- pure pc' `orDie` "can't parse credential file"

  notice $ "run peer" <+> pretty (AsBase58 (view peerSignPk pc))

  s <- simpleStorageInit @HbSync (Just pref)
  let blk = liftIO . hasBlock s


  w <- replicateM defStorageThreads $ async $ liftIO $ simpleStorageWorker s

  localMulticast <- liftIO $ (headMay <$> parseAddrUDP (fromString defLocalMulticast)
                                      <&> fmap (fromSockAddr @'UDP . addrAddress) )

                           `orDie` "assertion: localMulticastPeer not set"

  notice $ "multicast:" <+> pretty localMulticast

  mess <- newMessagingUDP False listenSa
            `orDie` "unable listen on the given addr"

  udp <- async $ runMessagingUDP mess

  udp1 <- newMessagingUDP False rpcSa
            `orDie` "Can't start RPC listener"

  mrpc <- async $ runMessagingUDP udp1

  mcast <- newMessagingUDPMulticast defLocalMulticast
            `orDie` "Can't start RPC listener"

  messMcast <- async $ runMessagingUDP mcast

  brains <- newBasicBrains @e conf

  brainsThread <- async $ runBasicBrains conf brains

  denv <- newDownloadEnv brains

  let tcpListen = cfgValue @PeerListenTCPKey conf & fromMaybe ""
  let addr' = fromStringMay @(PeerAddr L4Proto) tcpListen

  trace $ "TCP addr:" <+> pretty tcpListen <+> pretty addr'

  tcp <- maybe1 addr' (pure Nothing) $ \addr -> do
           tcpEnv <- newMessagingTCP addr <&> set tcpOnClientStarted (onClientTCPConnected brains)
           -- FIXME: handle-tcp-thread-somehow
           void $ async $ runMessagingTCP tcpEnv
           pure $ Just tcpEnv

  (proxy, penv) <- mdo
      proxy <- newProxyMessaging mess tcp >>= \peer -> pure peer
            { _proxy_getEncryptionKey = \peer -> do
                  mpeerData <- withPeerM penv $ find (KnownPeerKey peer) id
                  mkey <- join <$> forM mpeerData \peerData -> getEncryptionKey penv peerData
                  case mkey of
                      Nothing ->
                          trace1 $ "ENCRYPTION empty getEncryptionKey"
                              <+> pretty peer <+> viaShow mpeerData
                      Just k ->
                          trace1 $ "ENCRYPTION success getEncryptionKey"
                              <+> pretty peer <+> viaShow mpeerData <+> viaShow k
                  pure mkey

            , _proxy_clearEncryptionKey = \peer -> do
                  mpeerData <- withPeerM penv $ find (KnownPeerKey peer) id
                  forM_ mpeerData \peerData -> setEncryptionKey penv peer peerData Nothing
                  -- deletePeerAsymmKey brains peer
                  forM_ mpeerData \peerData ->
                      deletePeerAsymmKey' brains (show peerData)

            , _proxy_sendResetEncryptionKeys = \peer -> withPeerM penv do
                  sendResetEncryptionKeys peer

            , _proxy_sendBeginEncryptionExchange = \peer -> withPeerM penv do
                  sendBeginEncryptionExchange pc
                      ((pubKeyFromKeypair @s . view envAsymmetricKeyPair) penv)
                      peer

          }
      penv <- newPeerEnv (AnyStorage s) (Fabriq proxy) (getOwnPeer mess)
      pure (proxy, penv)

  proxyThread <- async $ runProxyMessaging proxy

  let peerMeta = mkPeerMeta conf penv

  nbcache <- liftIO $ Cache.newCache (Just $ toTimeSpec ( 600 :: Timeout 'Seconds))

  void $ async $ forever do
    pause @'Seconds 600
    liftIO $ Cache.purgeExpired nbcache

  rce <- refChanWorkerEnv conf penv denv

  let refChanAdapter = RefChanAdapter
                           { refChanOnHead = refChanOnHeadFn rce
                           , refChanSubscribed = isPolledRef @e brains
                           , refChanWriteTran = refChanWriteTranFn rce
                           , refChanValidatePropose = refChanValidateTranFn @e rce
                           , refChanNotifyRely = refChanNotifyRelyFn @e rce
                           }

  rcw <- async $ liftIO $ runRefChanRelyWorker rce refChanAdapter

  let pexFilt pips = do
        tcpex <- listTCPPexCandidates @e brains <&> HashSet.fromList
        fset  <- forM pips $ \p -> do
                   toPeerAddr p >>= \case
                    (L4Address UDP _) -> pure $ Just p
                    pa@(L4Address TCP _) | HashSet.member pa tcpex -> pure $ Just p
                    _ -> pure Nothing

        pure (catMaybes fset)

  let onNoBlock (p, h) = do
        already <- liftIO $ Cache.lookup nbcache (p,h) <&> isJust
        unless already do
          mpde <- find (KnownPeerKey p) id
          maybe1 mpde none $ \pd -> do
            let pk = view peerSignKey pd
            when (Set.member pk helpFetchKeys) do
              liftIO $ Cache.insert nbcache (p,h) ()
              -- debug  $ "onNoBlock" <+> pretty p <+> pretty h
              withPeerM penv $ withDownload denv (addDownload mzero h)

  loop <- liftIO $ async do

            runPeerM penv $ do
              adapter <- mkAdapter


              reflogAdapter <- RefLog.mkAdapter
              reflogReqAdapter <- RefLog.mkRefLogRequestAdapter @e


              let doDownload h = do
                    pro <- isReflogProcessed @e brains h
                    if pro then do
                      withPeerM penv $ withDownload denv (addDownload mzero h)
                    else do
                      withPeerM penv $ withDownload denv (processBlock h)
                      setReflogProcessed @e brains h

              let doFetchRef puk = do
                   withPeerM penv $ do
                     forKnownPeers @e $ \p _ -> do
                       request p (RefLogRequest @e puk)

              let rwa = RefLog.RefLogWorkerAdapter
                   { RefLog.reflogDownload = doDownload
                   , RefLog.reflogFetch = doFetchRef
                   }

              let addNewRtt (p,rttNew) = withPeerM penv $ void $ runMaybeT do
                    def <- newPeerInfo
                    tv <- lift $ fetch True def (PeerInfoKey p) (view peerRTTBuffer)
                    insertRTT rttNew tv

              let hshakeAdapter = PeerHandshakeAdapter addNewRtt

              let encryptionHshakeAdapter ::
                      ( MonadIO m
                      , EventEmitter e (PeerAsymmInfo e) m
                      ) => EncryptionHandshakeAdapter L4Proto m s
                  encryptionHshakeAdapter = EncryptionHandshakeAdapter
                    { encHandshake_considerPeerAsymmKey = \peer mpubkey -> withPeerM penv do
                          mpeerData <- withPeerM penv $ find (KnownPeerKey peer) id
                          case mpubkey of
                              Nothing -> do
                                  -- trace $ "ENCRYPTION delete key" <+> pretty peer <+> viaShow mpeerData
                                  -- deletePeerAsymmKey brains peer
                                  forM_ mpeerData \peerData ->
                                      deletePeerAsymmKey' brains (show peerData)
                              Just pk -> do
                                  -- emit PeerAsymmInfoKey (PeerAsymmPubKey peer pk)
                                  let symmk = genCommonSecret @s
                                          (privKeyFromKeypair @s (view envAsymmetricKeyPair penv))
                                          pk
                                  case mpeerData of
                                      Nothing -> do
                                          -- insertPeerAsymmKey brains peer pk symmk
                                          -- insertPeerAsymmKey' brains (show peer) pk symmk
                                          trace $ "ENCRYPTION can not store key. No peerData"
                                              <+> pretty peer <+> viaShow mpeerData
                                      Just peerData -> do
                                          liftIO $ setEncryptionKey penv peer peerData (Just symmk)
                                          insertPeerAsymmKey' brains (show peerData) pk symmk

                    }

              -- dialReqProtoAdapter <- do
              --     dialReqProtoAdapterRouter <- pure dialogRoutes
              --     pure DialReqProtoAdapter {..}

              env <- ask

              pnonce <- peerNonce @e

              pl <- getPeerLocator @e

              addPeers @e pl ps

              subscribe @e PeerExpiredEventKey \(PeerExpiredEvent peer {-mpeerData-}) -> liftIO do
                  mpeerData <- withPeerM penv $ find (KnownPeerKey peer) id
                  forM_ mpeerData \peerData -> setEncryptionKey penv peer peerData Nothing
                  -- deletePeerAsymmKey brains peer
                  forM_ mpeerData \peerData ->
                      deletePeerAsymmKey' brains (show peerData)

              subscribe @e PeerAnnounceEventKey $ \(PeerAnnounceEvent pip nonce) -> do
               unless (nonce == pnonce) $ do
                 debug $ "Got peer announce!" <+> pretty pip
                 mpd :: Maybe (PeerData e) <- find (KnownPeerKey pip) id
                 banned <- maybe (pure False) (peerBanned pip) mpd
                 let known = isJust mpd && not banned
                 sendPing pip

              subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
                 pa <- toPeerAddr p
                 liftIO $ atomically $ writeTQueue rpcQ (CHECK no pa (view biHash bi))

              subscribe @e AnyKnownPeerEventKey $ \(KnownPeerEvent p pd) -> do

                let thatNonce = view peerOwnNonce pd

                now <- liftIO getTimeCoarse

                -- defPeerInfo <- newPeerInfo
                -- fetch True defPeerInfo (PeerInfoKey p) id >>= \pinfo -> do

                find (PeerInfoKey p) id >>= mapM_ \pinfo -> do
                      liftIO $ atomically $ writeTVar (view peerPingFailed pinfo) 0
                      liftIO $ atomically $ writeTVar (view peerLastWatched pinfo) now

                banned <- peerBanned p pd

                let doAddPeer p = do
                      addPeers pl [p]

                      -- TODO: better-handling-for-new-peers
                      npi    <- newPeerInfo

                      here <- find @e (KnownPeerKey p) id <&> isJust

                      unless here do
                        debug $ "Got authorized peer!" <+> pretty p
                                                       <+> pretty (AsBase58 (view peerSignKey pd))
                        request @e p (GetPeerMeta @e)


                -- FIXME: check if we've got a reference to ourselves
                if | pnonce == thatNonce -> do
                    debug $ "GOT OWN NONCE FROM" <+> pretty p
                    delPeers pl [p]
                    addExcluded pl [p]
                    expire (KnownPeerKey p)

                   | banned -> do
                       notice $ pretty p <+> "banned"

                   | otherwise -> do

                    update pd (KnownPeerKey p) id

                    pdkv :: Map BS.ByteString (Peer L4Proto) <- fmap (Map.fromList . catMaybes)
                        $ knownPeers @e pl >>= mapM \pip ->
                              fmap (, pip) <$> find (KnownPeerKey pip) (view peerOwnNonce)

                    let proto1 = view sockType p

                    case Map.lookup thatNonce pdkv of

                      -- TODO: prefer-local-peer-with-same-nonce-over-remote-peer
                      --   remove remote peer
                      --   add local peer

                      -- FIXME: move-protocol-comparison-to-peer-nonce
                      --

                      Nothing -> doAddPeer p

                      Just p0 -> do

                        pa0 <- toPeerAddr p0
                        pa1 <- toPeerAddr p

                        if | pa0 == pa1 -> none
                           | view sockType p0 /= view sockType p -> do
                              doAddPeer p

                           | otherwise -> do

                              debug "Same peer, different address"

                              void $ runMaybeT do

                                pinfo0 <- MaybeT $ find (PeerInfoKey p0) id
                                pinfo1 <- MaybeT $ find (PeerInfoKey p) id

                                rtt0 <- MaybeT $ medianPeerRTT pinfo0
                                rtt1 <- MaybeT $ medianPeerRTT pinfo1

                                when ( rtt1 < rtt0 ) do
                                  debug $ "Better rtt!" <+> pretty p0
                                                        <+> pretty p
                                                        <+> pretty rtt0
                                                        <+> pretty rtt1

                                  lift $ do
                                    expire (KnownPeerKey p0)
                                    delPeers pl [p0]
                                    -- addExcluded pl [p0]
                                    doAddPeer p


              void $ liftIO $ async $ withPeerM env do
                pause @'Seconds 1
                debug "sending first peer announce"
                request localMulticast (PeerAnnounce @e pnonce)

              let peerThread t mx = W.tell . L.singleton =<< (liftIO . async) do
                    withPeerM env mx
                      `U.withException` \e -> case (fromException e) of
                        Just (e' :: AsyncCancelled) -> pure ()
                        Nothing -> err ("peerThread" <+> viaShow t <+> "Failed with" <+> viaShow e)
                    debug $ "peerThread Finished:" <+> t
              workers <- W.execWriterT do

                peerThread "local multicast" $ forever $ do
                  pause defPeerAnnounceTime -- FIXME: setting!
                  debug "sending local peer announce"
                  request localMulticast (PeerAnnounce @e pnonce)

                -- peerThread "tcpWorker" (tcpWorker conf)

                peerThread "httpWorker" (httpWorker conf peerMeta denv)

                peerThread "checkMetrics" (checkMetrics metrics)

                peerThread "peerPingLoop" (peerPingLoop @e conf penv)

                peerThread "knownPeersPingLoop" (knownPeersPingLoop @e conf)

                peerThread "bootstrapDnsLoop" (bootstrapDnsLoop @e conf)

                peerThread "pexLoop" (pexLoop @e brains tcp)

                peerThread "blockDownloadLoop" (blockDownloadLoop denv)

                peerThread "encryptionHandshakeWorker"
                    (EncryptionKeys.encryptionHandshakeWorker @e conf penv pc encryptionHshakeAdapter)

                let tcpProbeWait :: Timeout 'Seconds
                    tcpProbeWait = (fromInteger . fromMaybe 300) (cfgValue @PeerTcpProbeWaitKey conf)

                peerThread "fillPeerMeta" (fillPeerMeta tcp tcpProbeWait)

                -- FIXME: clumsy-code
                -- Is it better now ?
                when useHttpDownload do
                  peerThread "blockHttpDownloadLoop" (blockHttpDownloadLoop denv)

                peerThread "postponedLoop" (postponedLoop denv)

                peerThread "downloadQueue" (downloadQueue conf denv)

                peerThread "reflogWorker" (reflogWorker @e conf rwa)

                peerThread "refChanWorker" (refChanWorker @e rce (SomeBrains brains))

                peerThread "ping pong" $ forever $ do
                          cmd <- liftIO $ atomically $ readTQueue rpcQ
                          case cmd of
                            POKE -> debug "on poke: alive and kicking!"

                            PING pa r -> do
                              debug $ "ping" <+> pretty pa
                              pip <- fromPeerAddr @e pa
                              subscribe (ConcretePeerKey pip) $ \(ConcretePeerData _ pde) -> do

                                maybe1 r (pure ()) $ \rpcPeer -> do
                                  pinged <- toPeerAddr pip
                                  request rpcPeer (RPCPong @e pinged)
                                  -- case (view peerEncPubKey pde) of
                                  --     Nothing -> unencrypted ping
                                  --     Just pubkey -> encryptengd

                              sendPing pip

                            ANNOUNCE h  -> do
                              debug $ "got announce rpc" <+> pretty h
                              sto <- getStorage
                              mbsize <- liftIO $ hasBlock sto h

                              maybe1 mbsize (pure ()) $ \size -> do
                                debug "send multicast announce"

                                no <- peerNonce @e
                                let annInfo = BlockAnnounceInfo 0 NoBlockInfoMeta size h
                                let announce = BlockAnnounce @e no annInfo

                                request localMulticast announce

                                liftIO $ withPeerM env do
                                  forKnownPeers $ \p _ -> do
                                    debug $ "send single-cast announces" <+> pretty p
                                    request @e p announce

                            CHECK nonce pa h -> do
                              pip <- fromPeerAddr @e pa

                              n1 <- peerNonce @e

                              unless (nonce == n1) do

                                mpde <- find @e (KnownPeerKey pip) id

                                debug $ "received announce from"
                                              <+> pretty pip
                                              <+> pretty h

                                case mpde of
                                  Nothing -> do
                                    sendPing @e pip
                                    -- TODO: enqueue-announce-from-unknown-peer?

                                  Just pd  -> do

                                    banned <- peerBanned pip pd

                                    notAccepted <- acceptAnnounce pip pd <&> not

                                    if | banned -> do

                                          notice $ pretty pip <+> "banned"

                                       | notAccepted -> do

                                          debug $ pretty pip <+> "announce-not-accepted"

                                       | otherwise -> do

                                          downloadLogAppend @e h
                                          withDownload denv $ do
                                            processBlock h

                            REFLOGUPDATE bs -> do

                              trace "REFLOGUPDATE"

                              let msg' = deserialiseOrFail @(RefLogUpdate L4Proto) bs
                                          & either (const Nothing) Just

                              when (isNothing msg') do
                                warn "unable to parse RefLogUpdate message"

                              maybe1 msg' none $ \msg -> do
                                let pubk = view refLogId msg
                                emit @e RefLogUpdateEvKey (RefLogUpdateEvData (pubk, msg))
                                RefLog.doRefLogBroadCast msg

                            _ -> pure ()


                peerThread "all protos" do
                  runProto @e
                    [ makeResponse (blockSizeProto blk dontHandle onNoBlock)
                    , makeResponse (blockChunksProto adapter)
                    , makeResponse blockAnnounceProto
                    , makeResponse (withCredentials @e pc . peerHandShakeProto hshakeAdapter penv)
                    , makeResponse (withCredentials @e pc . encryptionHandshakeProto encryptionHshakeAdapter penv)
                    , makeResponse (peerExchangeProto pexFilt)
                    , makeResponse (refLogUpdateProto reflogAdapter)
                    , makeResponse (refLogRequestProto reflogReqAdapter)
                    , makeResponse (peerMetaProto peerMeta)
                    , makeResponse (refChanHeadProto False refChanAdapter)
                    , makeResponse (refChanUpdateProto False pc refChanAdapter)
                    , makeResponse (refChanRequestProto False refChanAdapter)
                    , makeResponse (refChanNotifyProto False refChanAdapter)
                    -- , makeResponse (dialReqProto dialReqProtoAdapter)
                    ]

              void $ liftIO $ waitAnyCancel workers

  let dieAction _ = do
        liftIO $ die "received die command"

  let pokeAction _ = do
        who <- thatPeer (Proxy @(RPC e))
        let k = view peerSignPk pc
        let rpc  = "rpc:" <+> dquotes (pretty (listenAddr udp1))
        let udp  = "udp:" <+> dquotes (pretty (listenAddr mess))

        let http = case cfgValue @PeerHttpPortKey conf :: Maybe Integer of
                     Nothing -> mempty
                     Just p  -> "http-port:" <+> pretty p

        let answ = show $ vcat [ "peer-key:" <+> dquotes (pretty (AsBase58 k))
                               , rpc
                               , udp
                               , http
                               ]

        -- FIXME: to-delete-POKE
        liftIO $ atomically $ writeTQueue rpcQ POKE
        request who (RPCPokeAnswerFull @e (Text.pack answ))

  let annAction h = do
        liftIO $ atomically $ writeTQueue rpcQ (ANNOUNCE h)

  let pingAction pa = do
        that <- thatPeer (Proxy @(RPC e))
        liftIO $ atomically $ writeTQueue rpcQ (PING pa (Just that))

  let fetchAction h = do
        debug  $ "fetchAction" <+> pretty h
        liftIO $ withPeerM penv $ do
                  downloadLogAppend @e h
                  withDownload denv (processBlock h)

  let peersAction _ = do
        who <- thatPeer (Proxy @(RPC e))
        void $ liftIO $ async $ withPeerM penv $ do
          forKnownPeers @e $ \p pde -> do
              pa <- toPeerAddr p
              let k = view peerSignKey pde
              request who (RPCPeersAnswer @e pa k)

  let pexInfoAction :: RPC L4Proto -> ResponseM L4Proto (RpcM (ResourceT IO)) ()
      pexInfoAction _ = do
        who <- thatPeer (Proxy @(RPC e))
        void $ liftIO $ async $ withPeerM penv $ do
            -- FIXME: filter-pexinfo-entries
            ps <- getAllPex2Peers
            request who (RPCPexInfoAnswer @e ps)

  let logLevelAction = \case
        DebugOn True  -> do
          setLogging @DEBUG debugPrefix
          debug "DebugOn"

        DebugOn False -> do
          debug "DebugOff"
          setLoggingOff @DEBUG

        TraceOn True -> do
          setLogging @TRACE tracePrefix
          trace "TraceOn"

        TraceOn False -> do
          trace "TraceOff"
          setLoggingOff @TRACE

  let reflogUpdateAction bs = void $ runMaybeT do
        liftIO $ atomically $ writeTQueue rpcQ (REFLOGUPDATE bs)
        -- trace $ "reflogUpdateAction"
        --
  let reflogFetchAction puk = do
        trace "reflogFetchAction"
        void $ liftIO $ async $ withPeerM penv $ do
          broadCastMessage (RefLogRequest @e puk)

  let reflogGetAction puk = do
        trace $ "reflogGetAction" <+> pretty (AsBase58 puk)
        who <- thatPeer (Proxy @(RPC e))
        void $ liftIO $ async $ withPeerM penv $ do
          sto <- getStorage
          h <- liftIO $ getRef sto (RefLogKey @(Encryption e) puk)
          request who (RPCRefLogGetAnswer @e  h)

  let refChanHeadSendAction h = do
        trace $ "refChanHeadSendAction" <+> pretty h
        void $ liftIO $ async $ withPeerM penv $ do
          me <- ownPeer @e
          sto <- getStorage

          chunks <- S.toList_ $ do
            deepScan ScanDeep (const none) h (liftIO . getBlock sto) $ \ha -> do
              unless (ha == h) do
                blk <- liftIO $ getBlock sto ha
                maybe1 blk none S.yield

          let box = deserialiseOrFail @(SignedBox (RefChanHeadBlock e) e) (LBS.concat chunks)

          case box of
            Left{} -> err $ "can't read head block" <+> pretty h
            Right (SignedBox k _ _) -> do
              let msg = RefChanHead k (RefChanHeadBlockTran (HashRef h))
              refChanNotifyOnUpdated rce k
              runResponseM me $ refChanHeadProto @e True refChanAdapter msg

  let refChanHeadGetAction puk = do
        trace $ "refChanHeadGetAction" <+> pretty (AsBase58 puk)
        who <- thatPeer (Proxy @(RPC e))
        void $ liftIO $ async $ withPeerM penv $ do
          sto <- getStorage
          h <- liftIO $ getRef sto (RefChanHeadKey @(Encryption e) puk)
          request who (RPCRefChanHeadGetAnsw @e  h)

  let refChanHeadFetchAction puk = do
        trace "reChanFetchAction"
        void $ liftIO $ async $ withPeerM penv $ do
          broadCastMessage (RefChanGetHead @e puk)

  let refChanProposeAction (puk, lbs) = do
        trace "reChanProposeAction"
        void $ liftIO $ async $ withPeerM penv $ do
          me <- ownPeer @e
          runMaybeT do
            box <- MaybeT $ pure $ deserialiseOrFail lbs & either (const Nothing) Just
            proposed <- MaybeT $ makeProposeTran @e pc puk box

            -- debug $ "PROPOSAL:" <+> pretty (LBS.length (serialise proposed))
            -- lift $ broadCastMessage (Propose @e puk proposed)

            -- FIXME: remove-this-debug-stuff
            --   или оставить? нода будет сама себе
            --   консенсус слать тогда. может, и оставить
            lift $ runResponseM me $ refChanUpdateProto @e True pc refChanAdapter (Propose @e puk proposed)

  let refChanNotifyAction (puk, lbs) = do
        trace "refChanNotifyAction"
        void $ liftIO $ async $ withPeerM penv $ do
          me <- ownPeer @e
          runMaybeT do
            box <- MaybeT $ pure $ deserialiseOrFail lbs & either (const Nothing) Just
            lift $ runResponseM me $ refChanNotifyProto @e True refChanAdapter (Notify @e puk box)

  let refChanGetAction puk = do
        trace $ "refChanGetAction" <+> pretty (AsBase58 puk)
        who <- thatPeer (Proxy @(RPC e))
        void $ liftIO $ async $ withPeerM penv $ do
          sto <- getStorage
          h <- liftIO $ getRef sto (RefChanLogKey @(Encryption e) puk)
          trace $ "refChanGetAction ANSWER IS" <+> pretty h
          request who (RPCRefChanGetAnsw @e  h)

  let refChanFetchAction puk = do
        trace $ "refChanFetchAction" <+> pretty (AsBase58 puk)
        void $ liftIO $ async $ withPeerM penv $ do
          gossip (RefChanRequest @e puk)

  let arpc = RpcAdapter
          { rpcOnPoke          = pokeAction
          , rpcOnDie           = dieAction
          , rpcOnPokeAnswer    = dontHandle
          , rpcOnPokeAnswerFull = dontHandle
          , rpcOnAnnounce      = annAction
          , rpcOnPing          = pingAction
          , rpcOnPong          = dontHandle
          , rpcOnFetch         = fetchAction
          , rpcOnPeers         = peersAction
          , rpcOnPeersAnswer   = dontHandle
          , rpcOnPexInfo       = pexInfoAction
          , rpcOnPexInfoAnswer = dontHandle
          , rpcOnLogLevel      = logLevelAction
          , rpcOnRefLogUpdate  = reflogUpdateAction
          , rpcOnRefLogFetch   = reflogFetchAction
          , rpcOnRefLogGet     = reflogGetAction
          , rpcOnRefLogGetAnsw = dontHandle

          , rpcOnRefChanHeadSend = refChanHeadSendAction
          , rpcOnRefChanHeadGet = refChanHeadGetAction
          , rpcOnRefChanHeadGetAnsw = dontHandle
          , rpcOnRefChanHeadFetch = refChanHeadFetchAction

          , rpcOnRefChanFetch   = refChanFetchAction
          , rpcOnRefChanGet     = refChanGetAction
          , rpcOnRefChanGetAnsw = dontHandle -- rpcOnRefChanGetAnsw

          , rpcOnRefChanPropose = refChanProposeAction
          , rpcOnRefChanNotify = refChanNotifyAction
          }

  dialReqProtoAdapter <- do
      dialReqProtoAdapterRouter <- pure dialogRoutes
      pure DialReqProtoAdapter {..}

  rpc <- async $ runRPC udp1 do
                   runProto @e
                     [ makeResponse (rpcHandler arpc)
                     , makeResponse (dialReqProto dialReqProtoAdapter)
                     ]

  menv <- newPeerEnv (AnyStorage s) (Fabriq mcast) (getOwnPeer mcast)

  ann <- liftIO $ async $ runPeerM menv $ do

                   self <- ownPeer @e

                   subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
                    unless (p == self) do
                      pa <- toPeerAddr p
                      liftIO $ atomically $ writeTQueue rpcQ (CHECK no pa (view biHash bi))

                   subscribe @e PeerAnnounceEventKey $ \pe@(PeerAnnounceEvent pip nonce) -> do
                      -- debug $ "Got peer announce!" <+> pretty pip
                      emitToPeer penv PeerAnnounceEventKey pe

                   runProto @e
                     [ makeResponse blockAnnounceProto
                     , makeResponse peerAnnounceProto
                     ]

  void $ waitAnyCancel $ w <> [udp,loop,rpc,mrpc,ann,messMcast,brainsThread]

  liftIO $ simpleStorageStop s



emitToPeer :: ( MonadIO m
              , EventEmitter e a (PeerM e IO)
              )
           => PeerEnv e
           -> EventKey e a
           -> Event e a
           -> m ()

emitToPeer env k e = liftIO $ withPeerM env (emit k e)


