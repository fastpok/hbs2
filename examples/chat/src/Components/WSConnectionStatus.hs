module Components.WSConnectionStatus (wsConnectionStatus) where

import Components.Icons
import Lucid

wsConnectionStatus :: Html ()
wsConnectionStatus = div_ [id_ "ws-connection-status", class_ "ws-connection-status offline"] $ do
  makeIcon Circle
  span_ [id_ "ws-connection-status-text"] " offline"
