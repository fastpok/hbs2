module Pages.Main (mainPage) where

import Components.Head
import Components.Icons
import Components.LogoutButton
import Components.ThemeToggleButton
import Config
import Control.Monad
import Control.Monad.Reader
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
  config' <- lift $ asks config
  let refChans' = refChans config'
  html $ renderText $ do
    doctype_
    html_ [lang_ "en"] $ do
      htmlHead
      htmlBody refChans'

htmlBody :: [MyRefChan] -> Html ()
htmlBody refChans' = body_ [class_ "h-screen"] $ do
  initScript
  div_ [class_ "wrapper"] $ do
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
        div_ [class_ "messages", id_ "messages"] ""
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
                [class_ "outline send-message", handleSendMessageClick]
                $ makeIcon PaperAirplane
    div_ [class_ "members-header wrapper-item header-color"] "Members"
    div_ [class_ "members wrapper-item"] $ do
      pure ()

-- createMember "Гэндальф"
-- createMember "Арагорн"
-- createMember "Фродо"

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
  set #messages.innerText to ''
  set global chat to @data-value
  remove .active from .chat-button
  add .active to me
  add .hidden to #chat-placeholder
  remove .hidden from #chat
  set #chat-name.innerText to my.innerText
  send subscribe(chat: chat) to WebSocket
|]

-- TODO: scroll down automatically only when we already at bottom
initScript :: Html ()
initScript =
  script_ [type_ "text/hyperscript"] $
    toHtmlRaw @String
      [qc|
def initWebSocket()
  socket WebSocket /
    on message
      put message into #messages.innerHTML
      {scrollDown}
end

init
  if not localStorage.user
    go to url '/login'
  end
  initWebSocket()
|]

autoresizeMessageInput :: String
autoresizeMessageInput =
  [qc|
js autoResize(document.getElementById('message-input')) end
|]

scrollDown :: String
scrollDown =
  [qc|
js scrollDown(document.getElementById('messages')) end
|]

sendMessageTemplate :: Text -> String
sendMessageTemplate message =
  [qc|
send message(
  body: {message},
  chat: chat,
  author: localStorage.user
) to WebSocket
|]

handleMessageInput :: Attribute
handleMessageInput =
  hyper_
    [qc|
on input {autoresizeMessageInput}

on keydown[(key is 'Enter') and (not ctrlKey)]
  halt the event
  if my.value
    {sendMessageTemplate "my.value"}
    set my.value to ''
    {autoresizeMessageInput}
  end

on keydown[(key is 'Enter') and ctrlKey]
  pick items start to my.selectionStart from my.value
  set left to it
  pick items my.selectionEnd to end from my.value
  set right to it
  put (left + '\\n' + right) into my.value
  put (left.length + 1) into my.selectionStart
  put (left.length + 1) into my.selectionEnd
  {autoresizeMessageInput}
|]

handleSendMessageClick :: Attribute
handleSendMessageClick =
  hyper_
    [qc|
on click
  if #message-input.value
    {sendMessageTemplate "#message-input.value"}
    set #message-input.value to ''
    {autoresizeMessageInput}
  end
|]
