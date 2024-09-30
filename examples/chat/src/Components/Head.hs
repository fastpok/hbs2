module Components.Head (htmlHead) where

import Lucid

htmlHead :: Html ()
htmlHead = head_ $ do
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  meta_ [name_ "color-scheme", content_ "light dark"]
  -- This script should be run as early as possible to avoid color flashing occurring on page load.
  script_ [src_ "js/theme-toggle.js"] ("" :: String)
  script_ [src_ "js/htmx.min.js", defer_ ""] ("" :: String)
  script_ [src_ "js/hyperscript.min.js", defer_ ""] ("" :: String)
  script_ [src_ "js/simplebar.min.js", defer_ ""] ("" :: String)
  script_ [src_ "js/autoresize.js", defer_ ""] ("" :: String)
  link_ [rel_ "stylesheet", href_ "css/pico.min.css"]
  link_ [rel_ "stylesheet", href_ "css/pico.colors.min.css"]
  link_ [rel_ "stylesheet", href_ "css/simplebar.min.css"]
  link_ [rel_ "stylesheet", href_ "css/styles.css"]
  link_ [rel_ "stylesheet", href_ "css/username-colors.css"]
  link_ [rel_ "icon", type_ "image/x-icon", href_ "img/favicon.ico"]
  title_ "Chat"
