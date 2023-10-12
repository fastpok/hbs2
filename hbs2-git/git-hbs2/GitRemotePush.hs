{-# Language AllowAmbiguousTypes #-}
module GitRemotePush where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Net.Auth.Credentials hiding (getCredentials)

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2Git.Config as Config
import HBS2Git.Types
import HBS2Git.State
import HBS2Git.App
import HBS2Git.Export (exportRefOnly,exportRefDeleted)
import HBS2Git.Import (importRefLogNew)

import GitRemoteTypes

import Control.Monad.Reader
import Data.Functor
import Data.Set (Set)
import Text.InterpolatedString.Perl6 (qc)
import Control.Monad.Catch
import Control.Monad.Trans.Resource

newtype RunWithConfig m a =
  WithConfig { fromWithConf :: ReaderT [Syntax C] m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader [Syntax C]
                   , MonadTrans
                   , MonadThrow
                   , MonadCatch
                   , MonadMask
                   , MonadUnliftIO
                   )


runWithConfig :: MonadIO m => [Syntax C] -> RunWithConfig m a -> m a
runWithConfig conf m = runReaderT (fromWithConf m) conf


instance (Monad m, HasGlobalOptions m) => HasGlobalOptions (RunWithConfig m) where
  addGlobalOption k v = lift $ addGlobalOption k v
  getGlobalOption k = lift $ getGlobalOption k

instance (Monad m, HasStorage m) => HasStorage (RunWithConfig m) where
  getStorage = lift getStorage

instance (Monad m, HasRPC m) => HasRPC (RunWithConfig m) where
  getRPC = lift getRPC

instance MonadIO m => HasConf (RunWithConfig (GitRemoteApp m)) where
  getConf = ask

instance MonadIO m => HasRefCredentials (RunWithConfig (GitRemoteApp m)) where
  getCredentials = lift . getCredentials
  setCredentials r c = lift $ setCredentials r c


instance MonadIO m => HasEncryptionKeys (RunWithConfig (GitRemoteApp m)) where
  addEncryptionKey = lift . addEncryptionKey
  findEncryptionKey = lift . findEncryptionKey
  enumEncryptionKeys = lift enumEncryptionKeys

push :: forall  m . ( MonadIO m
                    , MonadCatch m
                    , HasConf m
                    , HasRefCredentials m
                    , HasEncryptionKeys m
                    , HasGlobalOptions m
                    , HasStorage m
                    , HasRPC m
                    , MonadUnliftIO m
                    , MonadMask m
                    )

     => RepoRef -> [Maybe GitRef] -> m (Maybe GitRef)


push remote what@[Just bFrom , Just br] = do

  _ <- cfgValue @ConfBranch  @(Set GitRef) <&> transformBi normalizeRef
  trace $ "PUSH PARAMS" <+> pretty what
  gh <- gitGetHash (normalizeRef bFrom) `orDie` [qc|can't read hash for ref {pretty br}|]
  _ <- traceTime "TIME: exportRefOnly" $ exportRefOnly () remote (Just bFrom) br gh
  importRefLogNew False remote
  pure (Just br)

push remote [Nothing, Just br]  = do

  _ <- cfgValue @ConfBranch  @(Set GitRef) <&> transformBi normalizeRef
  trace $ "deleting remote reference" <+> pretty br
  exportRefDeleted () remote br
  importRefLogNew False remote
  pure (Just br)

push r w = do
  warn $ "ignoring weird push" <+> pretty w <+> pretty r
  pure Nothing

