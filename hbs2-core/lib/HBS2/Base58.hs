module HBS2.Base58 where

import Data.ByteString.Base58 (encodeBase58, bitcoinAlphabet, decodeBase58,Alphabet(..))
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS

import Prettyprinter

newtype AsBase58 a = AsBase58 { unAsBase58 :: a }

alphabet :: Alphabet
alphabet = bitcoinAlphabet

getAlphabet :: [Char]
getAlphabet = BS8.unpack (unAlphabet alphabet)


toBase58 :: ByteString -> ByteString
toBase58 = encodeBase58 bitcoinAlphabet

fromBase58 :: ByteString -> Maybe ByteString
fromBase58 = decodeBase58 bitcoinAlphabet


instance Pretty (AsBase58 ByteString) where
  pretty (AsBase58 bs) = pretty $ BS8.unpack $ toBase58 bs

instance Pretty (AsBase58 LBS.ByteString) where
  pretty (AsBase58 bs) = pretty $ BS8.unpack $ toBase58 (LBS.toStrict bs)

instance Show (AsBase58 ByteString) where
  show (AsBase58 bs) = BS8.unpack $ toBase58 bs

