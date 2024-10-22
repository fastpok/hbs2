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

import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Data.Time
import Data.Time.Clock.POSIX
import HBS2.Prelude
import Lens.Micro.Mtl
import UnliftIO

getUTCTimeFromMessageTimestamp :: MessageTimestamp -> UTCTime
getUTCTimeFromMessageTimestamp (MessageTimestamp createdAt) = posixSecondsToUTCTime $ realToFrac createdAt

createMessage ::
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
createMessage CreateMessageServices{..} flags gks sender' rcpts' parts bs = do
    -- TODO: support-flags

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