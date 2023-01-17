{-# Language TypeFamilyDependencies  #-}
{-# Language UndecidableInstances #-}
module TestUniqProtoId where

import HBS2.Prelude
import HBS2.Prelude.Plated

import HasProtocol
import FakeMessaging

import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.Async
import Codec.Serialise hiding (encode,decode)

import Prettyprinter hiding (pipe)


data PingPong e = Ping Int
                | Pong Int
                deriving stock (Generic,Show,Read)

data PeekPoke e = Peek Int
                | Poke Int
                | Nop
                deriving stock (Generic,Show,Read)


instance Serialise (PingPong e)

instance Serialise (PeekPoke e)

instance HasProtocol Fake (PingPong Fake) where
  type instance ProtocolId (PingPong Fake) = 1
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance HasProtocol Fake (PeekPoke Fake) where
  type instance ProtocolId (PeekPoke Fake)  = 2
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

pingPongHandler :: forall e m  . ( MonadIO m
                                 , Response e (PingPong e) m
                                 , HasProtocol e (PingPong e)
                                 )
                => PingPong e
                -> m ()

pingPongHandler  =
  \case
    Ping c -> liftIO (print $ "effect: PING" <+> pretty c) >> response (Pong @e c)
    Pong c -> liftIO (print $ "effect: PONG" <+> pretty c) >> response (Ping @e (succ c))

peekPokeHandler :: forall e m  . ( MonadIO m
                                 , Response e (PeekPoke e) m
                                 , HasProtocol e (PeekPoke e)
                                 )
                => PeekPoke e
                -> m ()

peekPokeHandler =
  \case
    Peek c -> liftIO (print $ "effect: Peek" <+> pretty c) >> response (Poke @e c)
    Poke c -> liftIO (print $ "effect: Poke" <+> pretty c) >> response (Nop @e)
    Nop    -> liftIO (print $ pretty "effect: Nop") >> response (Peek @e 1)


testUniqProtoId :: IO ()
testUniqProtoId = do

  fake <- newFakeP2P True

  let peer0 = FakePeer 0
  let peer1 = FakePeer 1

  env0 <- newEnv peer0 fake
  env1 <- newEnv peer1 fake

  runEngineM env0 $ do
    request peer1 (Ping @Fake 0)

  runEngineM env1 $ do
    request peer0 (Peek @Fake 0)

  pip1 <- async $
    runPeer env0
      [ makeResponse pingPongHandler
      , makeResponse peekPokeHandler
      ]

  pip2 <- async $
    runPeer env1
      [ makeResponse pingPongHandler
      , makeResponse peekPokeHandler
      ]

  (_, e) <- waitAnyCatchCancel [pip1, pip2]

  print e


