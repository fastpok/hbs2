module Util.UserNameColor where

import Data.Hashable (hash)
import Data.Text (Text)

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