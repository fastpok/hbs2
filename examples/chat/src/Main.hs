module Main where

import Config
import Control.Monad.Reader
import Env
import UnliftIO
import Workers

main :: IO ()
main = do
  config <- getConfig
  env <- initEnv config
  workers <- runReaderT runWorkers env
  void $ waitAnyCatchCancel workers
