module Components.MenuButton (menuButton, backButton) where

import Components.Icons
import Lucid
import Text.InterpolatedString.Perl6 (qc)
import Util.Lucid.Attributes

menuButton :: Html ()
menuButton = details_ [id_ "menu-button", class_ "dropdown mb-0"] $ do
  summary_
    [ class_ "outline header-button"
    , role_ "button"
    , hanldeWindowResize
    ]
    $ makeIcon Menu
  ul_ do
    li_ $ button_ [class_ "menu-item-button", handleChats] "Chats"
    li_ $ button_ [class_ "menu-item-button", handleMembers] "Members"

-- NOTE: since we change styles on narrow screens by using this menu,
-- we need to reset styles back on wide screens
hanldeWindowResize :: Attribute
hanldeWindowResize =
  hyper_
    [qc|
init
  call handleWindowResize()
|]

handleChats :: Attribute
handleChats =
  hyper_
    [qc|
on click
  remove @open from #menu-button
  set .wrapper's *grid-template-columns to '1fr 0 0'
  set .sidebar-header's *display to 'flex'
  set .sidebar's *display to 'flex'
  set .content-header's *display to 'none'
  set .content's *display to 'none'
|]

handleMembers :: Attribute
handleMembers =
  hyper_
    [qc|
on click
  remove @open from #menu-button
  set .wrapper's *grid-template-columns to '0 0 1fr'
  set .content-header's *display to 'none'
  set .content's *display to 'none'
  set .members-header's *display to 'flex'
  set .members's *display to 'block'
|]

backButton :: Html ()
backButton = button_
  [ class_ "outline header-button"
  , id_ "back-button"
  , title_ "Back"
  , ariaLabel_ "auto"
  , handleBack
  ]
  $ do
    makeIcon X

handleBack :: Attribute
handleBack =
  hyper_
    [qc|
on click
  set .wrapper's *grid-template-columns to '0 1fr 0'
  set .sidebar-header's *display to 'none'
  set .sidebar's *display to 'none'
  set .content-header's *display to 'flex'
  set .content's *display to 'flex'
  set .members-header's *display to 'none'
  set .members's *display to 'none'
|]
