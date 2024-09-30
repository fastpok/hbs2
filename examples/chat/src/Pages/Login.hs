module Pages.Login (loginPage) where

import Components.Head
import Config
import Control.Monad.Reader
import Env
import HBS2.Prelude
import Lucid
import Monad
import Web.Scotty.Trans

loginPage :: ActionT AppM ()
loginPage = do
    c <- lift $ asks config
    html $ renderText $ do
        doctype_
        html_ [lang_ "en"] $ do
            htmlHead
            htmlBody c

htmlBody :: Config -> Html ()
htmlBody c = body_ $ do
    div_ [class_ "wrapper"] $ do
        mapM_ (p_ . toHtml . show . pretty) (sigils c)
