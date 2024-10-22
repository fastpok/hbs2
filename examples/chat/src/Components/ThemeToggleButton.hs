module Components.ThemeToggleButton (themeToggleButton) where

import Components.Icons
import Lucid
import Util.Attributes

themeToggleButton :: Html ()
themeToggleButton = button_
  [ class_ "outline header-button",
    id_ "theme-toggle",
    title_ "Toggles light & dark",
    ariaLabel_ "auto",
    ariaLive_ "polite"
  ]
  $ do
    makeIcon Sun
    makeIcon Moon