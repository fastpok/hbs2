module Components.LogoutButton (logoutButton) where

import Components.Icons
import Lucid
import Text.InterpolatedString.Perl6 (qc)
import Util.Attributes

logoutButton :: Html ()
logoutButton = button_
  [ class_ "outline header-button",
    title_ "Log out",
    ariaLabel_ "auto",
    ariaLive_ "polite",
    handleLogout
  ]
  $ do
    makeIcon Logout

handleLogout :: Attribute
handleLogout =
  hyper_
    [qc|
on click
call localStorage.removeItem('user')
go to url '/login'
|]
