module Message where

import HBS2.Peer.Proto.Mailbox.Message
import HBS2.Peer.Proto.Mailbox.Types

import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Net.Auth.GroupKeySymm

import HBS2.Base58
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema ()
import HBS2.OrDie

import Codec.Serialise
import Control.Monad.Reader
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.Time.Clock.POSIX
import Env
import HBS2.KeyMan.Keys.Direct
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Prelude
import HBS2.Storage
import Lens.Micro.Mtl
import Types
import UnliftIO

getUTCTimeFromMessageTimestamp :: MessageTimestamp -> UTCTime
getUTCTimeFromMessageTimestamp (MessageTimestamp createdAt) = posixSecondsToUTCTime $ realToFrac createdAt

myCreateMessage ::
  forall s m.
  (MonadUnliftIO m, s ~ HBS2Basic) =>
  CreateMessageServices s ->
  MessageFlags ->
  Maybe GroupSecret ->
  -- | sender
  Either HashRef (Sigil s) ->
  -- | recipients
  [PubKey 'Encrypt s] ->
  -- | message parts
  [HashRef] ->
  -- | payload
  ByteString ->
  m (Message s)
myCreateMessage CreateMessageServices{..} flags gks sender' rcpts' parts bs = do
  (senderSignKey, senderEncryptionKey) <- getSenderKeys

  gk <- generateGroupKey @s gks (senderEncryptionKey : rcpts')

  _gkMt <- generateGroupKey @s gks mempty

  KeyringEntry pk sk _ <-
    cmLoadKeyringEntry senderEncryptionKey
      >>= orThrow (NoKeyringFound (show $ pretty $ AsBase58 senderEncryptionKey))

  gks' <- lookupGroupKey sk pk gk & orThrow SenderNoAccesToGroupKey

  encrypted <- encryptBlock cmStorage gks' (Right gk) Nothing bs

  let content =
        MessageContent @s
          flags
          Set.empty
          (Right gk)
          -- TODO: check-if-parts-exists
          (Set.fromList parts)
          encrypted

  creds <-
    cmLoadCredentials senderSignKey
      >>= orThrow (NoCredentialsFound (show $ pretty $ AsBase58 senderSignKey))

  let ssk = view peerSignSk creds

  let box = makeSignedBox @s senderSignKey ssk content

  pure $ MessageBasic box
 where
  getSenderKeys = case sender' of
    Right si -> fromSigil Nothing si
    Left hs -> do
      si <- loadSigil @s cmStorage hs >>= orThrow (SigilNotFound hs)
      fromSigil (Just hs) si
  fromSigil h si = do
    (rcpt, SigilData{..}) <- unboxSignedBox0 (sigilData si) & orThrow (MalformedSigil h)
    pure (rcpt, sigilDataEncKey)

getMessageFromStorage ::
  (MonadUnliftIO m, MonadReader Env m, s ~ HBS2Basic) =>
  Bool ->
  MyRefChan ->
  MyHashRef ->
  m (Maybe (PubKey 'Sign s, MessageContent s, ByteString))
getMessageFromStorage addToDownloadQueue refChan messageHashRef = do
  storageAPI <- asks storageAPI
  let storage = AnyStorage (StorageClient storageAPI)
  maybeBlock <- getBlock storage (fromMyHashRef messageHashRef)
  messageDownloadQueue' <- asks messageDownloadQueue
  case maybeBlock of
    Nothing -> do
      when addToDownloadQueue do
        atomically $
          writeTQueue messageDownloadQueue' $
            MessageDownloadQueueItem
              { messageDownloadQueueItemRefChan = refChan
              , messageDownloadQueueItemHashRef = messageHashRef
              }
      pure Nothing
    Just block -> do
      encryptedMessage <- orThrowUser "invalid message format" (deserialiseOrFail block)
      readMessageResult <- myReadMessage encryptedMessage
      case readMessageResult of
        Nothing -> do
          -- TODO: it would be better not to try to read and deserialize this message from storage again
          atomically $
            writeTQueue messageDownloadQueue' $
              MessageDownloadQueueItem
                { messageDownloadQueueItemRefChan = refChan
                , messageDownloadQueueItemHashRef = messageHashRef
                }
          pure Nothing
        Just message -> pure $ Just message

specialMessageParser :: Parser SpecialMessage
specialMessageParser = do
  Atto.skipSpace
  _ <- Atto.string "/name"
  Atto.skipSpace
  arg <- Atto.takeText
  let strippedArg = T.strip arg
  if T.null strippedArg
    then fail "No arguments found"
    else return $ SpecialMessageSetName strippedArg

parseSpecialMessage :: ByteString -> Maybe SpecialMessage
parseSpecialMessage message = case deserialiseMessageData message of
  WSMessageText (WSTextMessage text) -> eitherToMaybe $ Atto.parseOnly specialMessageParser text
  _ -> Nothing

deserialiseMessageData :: ByteString -> WSMessage
deserialiseMessageData messageDataBS =
  case deserialiseOrFail $ BSL.fromStrict messageDataBS of
    Left (DeserialiseFailure _ _) -> do
      -- old message format was plain UTF-8 text
      case TE.decodeUtf8' messageDataBS of
        Left _e -> WSUnknownMessage "failed to decode the message"
        Right text -> WSMessageText $ WSTextMessage text
    Right message -> message

myReadMessage ::
  forall s m.
  ( MonadUnliftIO m
  , s ~ HBS2Basic
  ) =>
  Message s ->
  m (Maybe (PubKey 'Sign s, MessageContent s, ByteString))
myReadMessage message = do
  let readMessageServices = ReadMessageServices (liftIO . runKeymanClientRO . extractGroupKeySecret)
  result <- try $ readMessage readMessageServices message
  case result of
    -- most likely, the encryption keys haven't been downloaded yet
    Left ReadNoGroupKeyAccess -> pure Nothing
    Left e -> throwIO e
    Right y -> pure $ Just y
