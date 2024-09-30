module Config
  ( Config (..),
    getConfig,
    MySigil,
    MyRefChan,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString qualified as BS
import Data.Config.Suckless
import Data.Either
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.OrDie
import HBS2.Prelude
import System.Directory
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)
import Types

data Config = Config
  { sigils :: [MySigil],
    refChans :: [MyRefChan]
  }

defaultConfig :: String
defaultConfig =
  [qc|; sigil "sigil-1.txt"
; sigil "sigil-2.txt"
; refchan "<refchan-1-id>"
; refchan "<refchan-2-id>"
|]

data SigilCfgKey

data RefChanCfgKey

instance HasCfgKey SigilCfgKey (Set FilePath) where
  key = "sigil"

instance HasCfgKey RefChanCfgKey (Set String) where
  key = "refchan"

readConfig :: (MonadIO m) => FilePath -> m [Syntax C]
readConfig fn = liftIO (readFile fn) <&> fromRight mempty . parseTop

parseSigilFile :: (MonadIO m) => FilePath -> m (Sigil 'HBS2Basic)
parseSigilFile sigilFile =
  (liftIO (BS.readFile sigilFile) <&> parseSerialisableFromBase58 @(Sigil 'HBS2Basic))
    `orDie` "Failed to parse sigil"

parseRefChan :: (MonadIO m) => String -> m MyRefChan
parseRefChan refchan = pure (fromStringMay @MyRefChan refchan) `orDie` "Failed to parse refchan"

parseConfig :: (MonadIO m, HasConf m) => m Config
parseConfig = do
  sigilFiles <- cfgValue @SigilCfgKey @(Set FilePath)
  refchanStrings <- cfgValue @RefChanCfgKey @(Set String)
  sigils <- mapM parseSigilFile (Set.toList sigilFiles)
  refChans <- mapM parseRefChan (Set.toList refchanStrings)
  let config =
        Config
          { sigils = sigils,
            refChans = refChans
          }
  pure config

getConfig :: (MonadIO m) => m Config
getConfig = do
  configDir <- liftIO $ getXdgDirectory XdgConfig "hbs2-chat"
  liftIO $ createDirectoryIfMissing True configDir
  let configPath = configDir </> "config"
  fileExists <- liftIO $ doesFileExist configPath
  unless fileExists do
    liftIO $ appendFile configPath defaultConfig
  syntax <- readConfig configPath
  runReaderT parseConfig syntax
