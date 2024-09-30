module Monad where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Env

newtype AppM a = AppM
    { runAppM :: ReaderT Env IO a
    }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadUnliftIO
        )