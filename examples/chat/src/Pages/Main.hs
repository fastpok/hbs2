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
import Util.Text
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
htmlBody refChans' = body_
  [ class_ "h-screen",
    hxExt_ "ws",
    wsConnect_ "/",
    wsSend_ "",
    hxTrigger_ "htmx:wsOpen",
    hxVals_ "js:{\"type\": \"hello\", \"client\": getUserSigil()}",
    -- hxDisinherit_ doesn't seem to work
    -- https://github.com/bigskysoftware/htmx/issues/1119
    hxDisinherit_ "hx-vals",
    handleWSMessages
  ]
  $ do
    initScript
    div_ [class_ "wrapper"] $ do
      div_ [class_ "sidebar-header wrapper-item header-color"] "Chats"
      div_ [class_ "sidebar wrapper-item chat-buttons"] $ do
        case refChans' of
          [] -> p_ $ small_ "There are no chats available"
          someRefChans -> forM_ someRefChans $ \refChan ->
            let refChanText = Text.pack $ show $ pretty $ AsBase58 refChan
                refChanShortenedText = shorten 8 refChanText
             in button_
                  [ class_ "outline chat-button",
                    wsSend_ "",
                    hxVals_ $ "{\"type\": \"active-chat\", \"chat\": \"" <> refChanText <> "\"}",
                    handleChatSelect refChanText
                  ]
                  $ toHtml refChanShortenedText
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
            form_ [id_ "message-form", wsSend_ "", hxVals_ "{\"type\": \"message\"}"] $
              fieldset_ [role_ "group", class_ "mb-0"] $
                do
                  textarea_
                    [ class_ "message-input",
                      id_ "message-input",
                      name_ "message",
                      placeholder_ "Message",
                      ariaLabel_ "Message",
                      required_ "",
                      rows_ "1",
                      handleMessageInput
                    ]
                    ""
                  button_
                    [class_ "outline send-message", type_ "submit"]
                    $ makeIcon PaperAirplane
      div_ [class_ "members-header wrapper-item header-color"] "Members"
      div_ [class_ "members wrapper-item", id_ "members"] ""

initScript :: Html ()
initScript =
  script_ [type_ "text/hyperscript"] $
    toHtmlRaw @String
      [qc|
init
  if not localStorage.user
    go to url '/login'
  end
|]

handleChatSelect :: Text -> Attribute
handleChatSelect refChanText =
  hyper_
    [qc|
on click
  set #messages.innerHTML to ''
  set #members.innerHTML to ''
  set global chat to '{refChanText}'
  remove .active from .chat-button
  add .active to me
  add .hidden to #chat-placeholder
  remove .hidden from #chat
  set #chat-name.innerText to my.innerText
|]

handleWSMessages :: Attribute
handleWSMessages =
  hyper_
    [qc|
on htmx:wsAfterMessage
  call handleIncomingWSMessage(event.detail.message)

on htmx:wsAfterSend
  call handleOutgoingWSMessage(event.detail.message)
|]

autoresizeMessageInput :: String
autoresizeMessageInput =
  [qc|
js autoResize(document.getElementById('message-input')) end
|]

handleMessageInput :: Attribute
handleMessageInput =
  hyper_
    [qc|
on input {autoresizeMessageInput}

on keydown[(key is 'Enter') and (not ctrlKey)]
  halt the event
  if my.value
    send submit to #message-form
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

on submit from #message-form
  set my.value to ''
  {autoresizeMessageInput}
|]
