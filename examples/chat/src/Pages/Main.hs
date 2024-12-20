module Pages.Main (mainPage) where

import Components.Head
import Components.Icons
import Components.LogoutButton
import Components.MenuButton
import Components.ThemeToggleButton
import Components.WSConnectionStatus
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
import Types
import Util.Lucid.Attributes
import Util.Lucid.Elements
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

htmlBody :: [NamedRefChan] -> Html ()
htmlBody refChans' = body_
  [ class_ "h-dvh"
  , hxExt_ "ws"
  , wsConnect_ "/"
  , wsSend_ ""
  , hxTrigger_ "htmx:wsOpen"
  , hxVals_ "js:{type: \"hello\", client: getUserSigil()}"
  , -- hxDisinherit_ doesn't seem to work
    -- https://github.com/bigskysoftware/htmx/issues/1119
    hxDisinherit_ "hx-vals"
  , handleWSAfterMessage
  , handleWSClose
  , handleWSOpen
  , handlePaste
  ]
  $ do
    initScript
    div_ [class_ "wrapper"] $ do
      div_ [class_ "sidebar-header wrapper-item header-color"] do
        "Chats"
        backButton
      div_ [class_ "sidebar wrapper-item chats"] $ do
        case refChans' of
          [] -> p_ $ small_ "There are no chats available"
          someRefChans -> forM_ someRefChans $ \namedRefChan -> div_ [class_ "chat-item"] $ do
            let refChanKeyText = Text.pack $ show $ pretty $ AsBase58 $ namedRefChanKey namedRefChan
            button_
              [ class_ "outline chat-button"
              , wsSend_ ""
              , hxVals_ $ "{\"type\": \"active-chat\", \"chat\": \"" <> refChanKeyText <> "\"}"
              , handleChatSelect
              ]
              $ toHtml
              $ namedRefChanName namedRefChan
            let copyTooltipText = "Copy chat ID"
            button_
              [ class_ "outline secondary copy-chat-id-button"
              , data_ "tooltip" copyTooltipText
              , onClickCopy copyTooltipText refChanKeyText
              ]
              $ makeIcon Copy
      div_ [class_ "content-header wrapper-item header-color"] $ do
        div_ [id_ "chat-name"] ""
        div_ [class_ "header-right"] $ do
          wsConnectionStatus
          themeToggleButton
          logoutButton
          menuButton

      div_ [class_ "content wrapper-item"] $ do
        div_ [id_ "chat-placeholder"] $ p_ "Select a chat"
        div_ [class_ "hidden", id_ "chat"] $ do
          div_ [class_ "messages", id_ "messages"] ""
          div_ [class_ "message-input-wrapper"] $
            form_ [id_ "message-form", wsSend_ "", hxVals_ "{\"type\": \"text-message\"}"] $
              fieldset_ [id_ "message-input-fieldset", role_ "group", class_ "mb-0"] $
                do
                  textarea_
                    [ class_ "message-input"
                    , id_ "message-input"
                    , name_ "message"
                    , placeholder_ "Message"
                    , ariaLabel_ "Message"
                    , required_ ""
                    , rows_ "1"
                    , handleMessageInput
                    ]
                    ""
                  button_
                    [ class_ "outline message-form-button send-files"
                    , type_ "button"
                    , data_ "target" "send-files-modal"
                    , onclick_ "toggleModal(event); document.getElementById('files-message-form').reset();"
                    ]
                    $ makeIcon PaperClip
                  button_
                    [class_ "outline message-form-button send-message", type_ "submit"]
                    $ makeIcon PaperAirplane
      div_ [class_ "members-header wrapper-item header-color"] do
        "Members"
        backButton
      div_ [class_ "members wrapper-item", id_ "members"] ""
      sendFilesModal

sendFilesModal :: Html ()
sendFilesModal = dialog_ [id_ "send-files-modal"] $
  article_ $ do
    header_ $ do
      button_
        [ ariaLabel_ "Close"
        , rel_ "prev"
        , data_ "target" "send-files-modal"
        , onclick_ "toggleModal(event)"
        ]
        ""
      h3_ "Send files"
    form_
      [ id_ "files-message-form"
      , wsSend_ ""
      , hxVals_ "{\"type\": \"files-message\"}"
      , data_ "target" "send-files-modal"
      , onsubmit_ "toggleModal(event)"
      , handleFilesMessageWSConfigSend
      ]
      $ input_
        [ id_ "files-input"
        , type_ "file"
        , name_ "filesUpload"
        , multiple_ ""
        , required_ ""
        , autofocus_
        ]
    footer_ $ do
      button_
        [ role_ "button"
        , class_ "secondary"
        , data_ "target" "send-files-modal"
        , onclick_ "toggleModal(event)"
        ]
        "Cancel"
      button_
        [ id_ "send-files-submit-button"
        , type_ "submit"
        , form_ "files-message-form"
        , data_ "target" "send-files-modal"
        ]
        "Confirm"

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

handleChatSelect :: Attribute
handleChatSelect =
  hyper_
    [qc|
on click
  set #messages.innerHTML to ''
  set #members.innerHTML to ''
  remove .active from .chat-button
  add .active to me
  add .hidden to #chat-placeholder
  remove .hidden from #chat
  set #chat-name.innerText to my.innerText
  set $activeChatSelected to true
|]

onClickCopy :: Text -> Text -> Attribute
onClickCopy tooltip s =
  hyper_
    [qc|
on click writeText('{s}') into the navigator's clipboard
  set my innerHTML to '{getIconSvg CopyCheck}'
  set @data-tooltip to 'Copied!'
  wait 2s
  set my innerHTML to '{getIconSvg Copy}'
  set @data-tooltip to '{tooltip}'
|]

handlePaste :: Attribute
handlePaste =
  hyper_
    [qc|
on paste
  if $activeChatSelected 
    call handlePaste(event)
  end
|]

handleWSAfterMessage :: Attribute
handleWSAfterMessage =
  hyper_
    [qc|
on htmx:wsAfterMessage
  call handleIncomingWSMessage(event.detail.message)
|]

handleWSClose :: Attribute
handleWSClose =
  hyper_
    [qc|
on htmx:wsClose or htmx:wsError
  set #messages.innerHTML to ''
  set #members.innerHTML to ''
  remove .active from .chat-button
  add @disabled to .chat-button
  add .hidden to #chat
  remove .hidden from #chat-placeholder
  set #chat-name.innerText to ''
  set #ws-connection-status-text.innerText to 'offline'
  add .offline to #ws-connection-status
  set $activeChatSelected to false
|]

handleWSOpen :: Attribute
handleWSOpen =
  hyper_
    [qc|
on htmx:wsOpen
  set #ws-connection-status-text.innerText to 'online'
  remove .offline from #ws-connection-status
  remove @disabled from .chat-button
|]

autoresizeMessageInput :: String
autoresizeMessageInput =
  [qc|
js autoResize(document.getElementById('message-input'), document.getElementById('message-input-fieldset')) end
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

handleFilesMessageWSConfigSend :: Attribute
handleFilesMessageWSConfigSend =
  hyper_
    [qc|
on htmx:wsConfigSend
  call handleFilesMessageWSConfigSend(event)
|]