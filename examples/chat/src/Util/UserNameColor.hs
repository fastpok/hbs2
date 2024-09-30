module Util.UserNameColor where

import Config
import Data.Hashable (hash)
import Data.Text (Text)
import Util.Attributes
import Web.Scotty.Trans

colorNames :: [Text]
colorNames =
  [ "red",
    "pink",
    "fuchsia",
    "purple",
    "violet",
    "indigo",
    "blue",
    "azure",
    "cyan",
    "jade",
    "green",
    "lime",
    "yellow",
    "amber",
    "pumpkin",
    "orange",
    "sand",
    "grey",
    "zinc",
    "slate"
  ]

userNameToColorClass :: Text -> Text
userNameToColorClass userName =
  "username-" <> color
  where
    color = colorNames !! (hash userName `mod` length colorNames)