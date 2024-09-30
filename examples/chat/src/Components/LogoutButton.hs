module Components.LogoutButton (logoutButton) where

import Components.Icons
import Lucid
import Utils.Attributes

logoutButton :: Html ()
logoutButton = button_
    [ class_ "outline header-button"
    , title_ "Log out"
    , ariaLabel_ "auto"
    , ariaLive_ "polite"
    , handleLogout
    ]
    $ do
        makeIcon Logout

handleLogout :: Attribute
handleLogout = hyper_ "on click js localStorage.removeItem('user') end then go to url '/login'"
