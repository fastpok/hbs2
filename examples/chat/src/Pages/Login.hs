module Pages.Login (loginPage) where

import Components.Head
import Components.ThemeToggleButton
import Config
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Env
import HBS2.Base58
import HBS2.Net.Auth.Credentials.Sigil (Sigil (..))
import Lucid
import Monad
import Prettyprinter
import Text.InterpolatedString.Perl6 (qc)
import Types
import Util.Attributes
import Web.Scotty.Trans

loginPage :: ActionT AppM ()
loginPage = do
  config' <- lift $ asks config
  let sigils' = sigils config'
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
                  let sigilSignPublicKey = T.pack $ show $ pretty $ AsBase58 $ sigilSignPk $ fromMySigil sigil
                      userData =
                        TL.toStrict $
                          Aeson.encodeToLazyText $
                            Aeson.object
                              [ "sigil" .= T.pack (show $ pretty $ AsBase58 $ fromMySigil sigil),
                                "publicKey" .= sigilSignPublicKey
                              ]
                   in option_ [value_ userData] $ toHtml sigilSignPublicKey
              button_ [class_ "whitespace-nowrap", handleLogin] "Log in"

noSigilsMessage :: Html ()
noSigilsMessage = do
  h2_ "No sigils available"
  p_ $ do
    "You can specify the sigils you want to use in the configuration file located at "
    code_ "~/.config/" <> toHtml appName <> "/config"

handleLogin :: Attribute
handleLogin =
  hyper_
    [qc|
on click
set localStorage.user to #user-select.value
go to url '/'
|]
