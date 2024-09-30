module Env (Env (..)) where

import Config

data Env = Env
    { config :: Config
    }