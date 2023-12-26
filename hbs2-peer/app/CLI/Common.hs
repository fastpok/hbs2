{-# Language TemplateHaskell #-}
module CLI.Common where

import HBS2.Clock
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto
import HBS2.Net.Proto.Service

import PeerConfig

import HBS2.Peer.RPC.Client.Unix

import Data.Kind
import Lens.Micro.Platform
import UnliftIO

data RPCOpt =
  RPCOpt
  { _rpcOptConf :: Maybe FilePath
  , _rpcOptAddr :: Maybe String
  }

makeLenses 'RPCOpt

withMyRPC :: forall (api :: [Type]) m . ( MonadUnliftIO m
                                        , HasProtocol UNIX (ServiceProto api UNIX)
                                        )
          => RPCOpt
          -> (ServiceCaller api UNIX -> m ())
          -> m ()

withMyRPC o m = do
  conf  <- peerConfigRead (view rpcOptConf o)
  let soname = getRpcSocketName conf
  withRPC2 @api  @UNIX soname m

withRPCMessaging :: MonadIO m => RPCOpt -> (MessagingUnix  -> m ()) -> m ()
withRPCMessaging o action = do
  conf  <- peerConfigRead (view rpcOptConf o)
  let soname = getRpcSocketName conf
  client1 <- newMessagingUnix False 1.0 soname
  m1 <- liftIO $ async $ runMessagingUnix client1
  link m1
  action client1
  pause @'Seconds 0.05
  cancel m1


