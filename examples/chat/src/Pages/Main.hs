module Pages.Main (mainPage) where

import Components.Head
import Components.Icons
import Components.LogoutButton
import Components.ThemeToggleButton
import Config
import Control.Monad.Reader
import Data.Hashable (hash)
import Data.Text (Text)
import Data.Text qualified as Text
import Env
import HBS2.Base58
import Lucid
import Monad
import Prettyprinter
import Text.InterpolatedString.Perl6 (qc)
import Util.Attributes
import Util.UserNameColor
import Web.Scotty.Trans

mainPage :: ActionT AppM ()
mainPage = do
  c <- lift $ asks config
  let refChans' = refChans c
  html $ renderText $ do
    doctype_
    html_ [lang_ "en"] $ do
      htmlHead
      htmlBody refChans'

htmlBody :: [MyRefChan] -> Html ()
htmlBody refChans' = body_ [class_ "h-screen"] $ do
  div_ [class_ "wrapper", checkLogin] $ do
    div_ [class_ "sidebar-header wrapper-item header-color"] "Chats"
    div_ [class_ "sidebar wrapper-item chat-buttons"] $ do
      case refChans' of
        [] -> p_ $ small_ "There are no chats available"
        someRefChans -> forM_ someRefChans $ \refChan ->
          let refChanText = Text.pack $ show $ pretty $ AsBase58 refChan
              refChanShortenedText = shorten 8 refChanText
           in button_ [class_ "outline chat-button", data_ "value" refChanText, handleChatSelect] $ toHtml refChanShortenedText
    div_ [class_ "content-header wrapper-item header-color"] $ do
      div_ [id_ "chat-name"] ""
      div_ [class_ "header-buttons"] $ do
        themeToggleButton
        logoutButton

    div_ [class_ "content wrapper-item"] $ do
      div_ [id_ "chat-placeholder"] $ p_ "Select a chat"
      div_ [class_ "hidden", id_ "chat"] $ do
        div_ [class_ "messages", id_ "messages", handlePolling] ""
        div_ [class_ "message-input-wrapper"] $
          fieldset_ [role_ "group", class_ "mb-0"] $
            do
              textarea_
                [ class_ "message-input",
                  id_ "message-input",
                  name_ "input",
                  placeholder_ "Message",
                  ariaLabel_ "Message",
                  rows_ "1",
                  handleMessageInput
                ]
                ""
              button_
                [class_ "outline send-message", handleSendMessageOnClick]
                $ makeIcon PaperAirplane
    div_ [class_ "members-header wrapper-item header-color"] "Members"
    div_ [class_ "members wrapper-item"] $ do
      createMember "Гэндальф"
      createMember "Арагорн"
      createMember "Фродо"

createMember :: Text -> Html ()
createMember username = p_ [class_ $ userNameToColorClass username] $ small_ $ toHtml username

shorten :: Int -> Text -> Text
shorten n t =
  if Text.length t > n
    then Text.take m t <> "..." <> Text.takeEnd m t
    else t
  where
    m = n `div` 2

handleChatSelect :: Attribute
handleChatSelect =
  hyper_
    [qc|
on click
send stoppolling to #messages
set #messages.innerText to ''
set global chat to @data-value
remove .active from .chat-button
add .active to me
add .hidden to #chat-placeholder
remove .hidden from #chat
set #chat-name.innerText to my.innerText
send startpolling to #messages
|]

checkLogin :: Attribute
checkLogin =
  hyper_
    [qc|
on load
if not localStorage.user
go to url '/login'
|]

handlePolling :: Attribute
handlePolling =
  hyper_
    [qc|
on startpolling 
repeat until event stoppolling
  fetch `/$\{chat}/messages`
  put the result into my.innerHTML
  wait 2s
end
|]

autoresizeMessageInput :: String
autoresizeMessageInput =
  [qc|
js autoResize(document.getElementById('message-input')) end
|]

postMessageTemplate :: Text -> String
postMessageTemplate message =
  [qc|
set requestBody to
\{
  author: localStorage.user,
  body: {message}
}
fetch `/$\{chat}/message` with
\{
  method:'POST',
  headers: \{'Content-Type': 'application/json'},
  body: JSON.stringify(requestBody)
}
|]

handleMessageInput :: Attribute
handleMessageInput =
  hyper_
    [qc|
on input {autoresizeMessageInput}

on keydown[key=='Enter' and (not event.ctrlKey)]
halt the event
{postMessageTemplate "my.value"}
set my.value to ''
{autoresizeMessageInput}

on keydown[key=='Enter' and event.ctrlKey]
put (my.value + '\\n') into my.value
{autoresizeMessageInput}
|]

handleSendMessageOnClick :: Attribute
handleSendMessageOnClick =
  hyper_
    [qc|
on click
{postMessageTemplate "#message-input.value"}
set #message-input.value to ''
{autoresizeMessageInput}
|]
