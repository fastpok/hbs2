module Pages.Login (loginPage) where

import Components.Head
import Components.ThemeToggleButton
import Config
import Control.Monad.Reader
import Data.Text qualified as Text
import Env
import Lucid
import Monad
import Prettyprinter
import Utils.Attributes
import Web.Scotty.Trans

loginPage :: ActionT AppM ()
loginPage = do
  c <- lift $ asks config
  let sigils' = sigils c
  html $ renderText $ do
    doctype_
    html_ [lang_ "en"] $ do
      htmlHead
      htmlBody sigils'

htmlBody :: [MySigil] -> Html ()
htmlBody sigils' = body_ [class_ "min-h-screen flex"] $
  div_ [class_ "container grow flex flex-col"] $ do
    div_ [class_ "content-header header-color"] $ do
      div_ "hbs2 chat"
      div_ [class_ "header-buttons"] $ do
        themeToggleButton
    div_ [class_ "grow login-wrapper"] $ do
      div_ $ do
        case sigils' of
          [] -> noSigilsMessage
          someSigils -> do
            h2_ "Select user"
            div_ [role_ "group"] $ do
              select_ [name_ "sigil", id_ "user-select", ariaLabel_ "Select user", required_ ""] $ do
                forM_ someSigils $ \sigil ->
                  let sigilText = Text.pack $ show $ pretty sigil
                   in option_ [value_ sigilText] $ toHtml sigilText
              button_ [class_ "whitespace-nowrap", handleLogin] "Log in"

noSigilsMessage :: Html ()
noSigilsMessage = do
  h2_ "No sigils available"
  p_ $ do
    "You can specify the sigils you want to use in the configuration file located at "
    code_ "~/.config/hbs2-chat/config"

handleLogin :: Attribute
handleLogin =
  hyper_
    "on click \
    \set localStorage.user to #user-select.value \
    \go to url '/'"
