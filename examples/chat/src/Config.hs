module Config
  ( Config (..),
    getConfig,
    MySigil,
    MyRefChan,
    appName,
  )
where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString qualified as BS
import Data.Config.Suckless
import Data.Either
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import HBS2.Net.Auth.Credentials
import HBS2.OrDie
import HBS2.Prelude
import System.Directory
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)
import Types

appName :: String
appName = "hbs2-chat"

data Config = Config
  { sigils :: [MySigil],
    refChans :: [MyRefChan],
    dbPath :: Maybe FilePath
  }

defaultConfig :: String
defaultConfig =
  [qc|; sigil "sigil-1.txt"
; sigil "sigil-2.txt"
; refchan "<refchan-1-id>"
; refchan "<refchan-2-id>"
; db-path <db-path>
|]

data SigilCfgKey

data RefChanCfgKey

data DBPath

instance HasCfgKey SigilCfgKey (Set FilePath) where
  key = "sigil"

instance HasCfgKey RefChanCfgKey (Set String) where
  key = "refchan"

instance HasCfgKey DBPath (Maybe FilePath) where
  key = "db-path"

readConfig :: (MonadUnliftIO m) => FilePath -> m [Syntax C]
readConfig fn = liftIO (readFile fn) <&> fromRight mempty . parseTop

parseSigilFile :: (MonadUnliftIO m) => FilePath -> m MySigil
parseSigilFile sigilFile =
  (liftIO (BS.readFile sigilFile) <&> parseSerialisableFromBase58)
    `orDie` "couldn't parse sigil"

parseRefChan :: (MonadUnliftIO m) => String -> m MyRefChan
parseRefChan refchan = pure (fromStringMay @MyRefChan refchan) `orDie` "couldn't parse refchan"

parseConfig :: (MonadUnliftIO m, HasConf m) => m Config
parseConfig = do
  sigilFiles <- cfgValue @SigilCfgKey @(Set FilePath)
  refchanStrings <- cfgValue @RefChanCfgKey @(Set String)
  dbPath <- cfgValue @DBPath @(Maybe FilePath)
  sigils <- mapM parseSigilFile (Set.toList sigilFiles)
  refChans <- mapM parseRefChan (Set.toList refchanStrings)
  pure $ Config {..}

getConfig :: (MonadUnliftIO m) => m Config
getConfig = do
  configDir <- liftIO $ getXdgDirectory XdgConfig appName
  liftIO $ createDirectoryIfMissing True configDir
  let configPath = configDir </> "config"
  fileExists <- liftIO $ doesFileExist configPath
  unless fileExists do
    liftIO $ appendFile configPath defaultConfig
  syntax <- readConfig configPath
  runReaderT parseConfig syntax
