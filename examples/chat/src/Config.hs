module Config (
  Config (..),
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
import Data.Text qualified as T
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
  { sigils :: [MySigil]
  , refChans :: [NamedRefChan]
  , dbPath :: Maybe FilePath
  }

defaultConfig :: String
defaultConfig =
  [qc|; sigil "sigil-1.txt"
; sigil "sigil-2.txt"
; refchan "<refchan-1-id>" "chat-name-1"
; refchan "<refchan-2-id>" "chat-name-2"
; db-path state.db
|]

data SigilCfgKey

data RefChanCfgKey

data DBPathKey

instance HasCfgKey SigilCfgKey (Set FilePath) where
  key = "sigil"

instance HasCfgKey RefChanCfgKey [NamedRefChan] where
  key = "refchan"

instance HasCfgKey DBPathKey (Maybe FilePath) where
  key = "db-path"

readConfig :: (MonadUnliftIO m) => FilePath -> m [Syntax C]
readConfig fn = liftIO (readFile fn) <&> fromRight mempty . parseTop

parseSigilFile :: (MonadUnliftIO m) => FilePath -> m MySigil
parseSigilFile sigilFile =
  (liftIO (BS.readFile sigilFile) <&> parseSerialisableFromBase58)
    `orDie` "couldn't parse sigil"

parseRefChan :: (MonadUnliftIO m) => Text -> m MyRefChan
parseRefChan refchan = pure (fromStringMay @MyRefChan (T.unpack refchan)) `orDie` "couldn't parse refchan"

makeNamedRefChan :: (MonadUnliftIO m) => Text -> Text -> m NamedRefChan
makeNamedRefChan refChan namedRefChanName = do
  namedRefChanKey <- parseRefChan refChan
  pure $ NamedRefChan{..}

getRefChansFromConfig :: (MonadUnliftIO m, HasConf m) => m [NamedRefChan]
getRefChansFromConfig = do
  syn <- getConf
  let xs =
        [ (refChan, refChanName)
        | ListVal (Key s [LitStrVal refChan, LitStrVal refChanName]) <- syn
        , s == key @RefChanCfgKey @[NamedRefChan]
        ]
  mapM (uncurry makeNamedRefChan) xs

parseConfig :: (MonadUnliftIO m, HasConf m) => m Config
parseConfig = do
  sigilFiles <- cfgValue @SigilCfgKey @(Set FilePath)
  dbPath <- cfgValue @DBPathKey @(Maybe FilePath)
  sigils <- mapM parseSigilFile (Set.toList sigilFiles)
  refChans <- getRefChansFromConfig
  pure $ Config{..}

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
