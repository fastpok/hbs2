{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
module HBS2.Net.Auth.AccessKey where

import HBS2.Base58
import HBS2.Data.Types
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Definition
import HBS2.Prelude.Plated

import Codec.Serialise
import Control.Monad ((<=<))
import Crypto.Saltine.Core.Box qualified as Encrypt
import Crypto.Saltine.Class qualified as Crypto
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 (ByteString)
import Data.List.Split (chunksOf)


type ForAccessKey s = ( Crypto.IsEncoding (PubKey 'Encrypt s)
                      , Serialise (PubKey 'Encrypt s)
                      , Serialise (PubKey 'Sign s)
                      , Serialise (PrivKey 'Sign s)
                      , Serialise (PrivKey 'Encrypt s)
                      )


newtype EncryptedBox = EncryptedBox { unEncryptedBox :: ByteString }
  deriving stock (Generic)

instance Serialise EncryptedBox

---

data family AccessKey s

newtype instance AccessKey s =
  AccessKeyNaClAsymm
  { permitted :: [(PubKey 'Encrypt s, EncryptedBox)]
  }
  deriving stock (Generic)

instance ForAccessKey s => Serialise (AccessKey s)

---

data family GroupKey s

data instance GroupKey s =
  GroupKeyNaClAsymm
  { recipientPk :: PubKey 'Encrypt s
  , accessKey :: AccessKey s
  }
  deriving stock (Generic)

instance ForAccessKey s => Serialise (GroupKey s)

---

newtype AsGroupKeyFile a = AsGroupKeyFile a

-- FIXME: integration-regression-test-for-groupkey
--   Добавить тест: сгенерировали groupkey/распарсили groupkey

parseGroupKey :: forall s . ForAccessKey s
                 =>  AsGroupKeyFile ByteString -> Maybe (GroupKey s)
parseGroupKey (AsGroupKeyFile bs) = parseSerialisableFromBase58 bs

instance ( Serialise  (GroupKey s)
         )

  =>  Pretty (AsBase58 (GroupKey s)) where
  pretty (AsBase58 c) =
    pretty . B8.unpack . toBase58 . LBS.toStrict . serialise $ c


instance Pretty (AsBase58 a) => Pretty (AsGroupKeyFile (AsBase58 a)) where
  pretty (AsGroupKeyFile pc) =  "# hbs2 groupkey file" <> line <> co
    where
      co = vcat $ fmap pretty
                $ chunksOf 60
                $ show
                $ pretty pc


parsePubKeys :: forall s . ForAccessKey s
             =>  ByteString
             -> Maybe [PubKey 'Encrypt s]

parsePubKeys = sequenceA . fmap (Crypto.decode <=< fromBase58) . B8.lines


-- FIXME: public-key-type-hardcode
--  Это нужно переместить в тайпкласс от s, аналогично Signatures
mkEncryptedKey :: forall s . (ForAccessKey s, PubKey 'Encrypt s ~ Encrypt.PublicKey)
               => KeyringEntry s
               -> PubKey 'Encrypt s
               -> IO EncryptedBox

mkEncryptedKey kr pk = EncryptedBox <$> Encrypt.boxSeal pk ((LBS.toStrict . serialise) kr)

openEncryptedKey :: forall s . ( ForAccessKey s
                               , PrivKey 'Encrypt s ~ Encrypt.SecretKey
                               , PubKey 'Encrypt s ~ Encrypt.PublicKey
                               )
                 => EncryptedBox
                 -> KeyringEntry s
                 -> Maybe (KeyringEntry s)

openEncryptedKey (EncryptedBox bs) kr =
    either (const Nothing) Just . deserialiseOrFail . LBS.fromStrict =<< Encrypt.boxSealOpen (_krPk kr) (_krSk kr) bs

