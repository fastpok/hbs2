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
import Utils.Attributes
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
htmlBody refChans' = body_ [class_ "h-screen", checkLogin] $ do
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
      div_ "Chat name"
      div_ [class_ "header-buttons"] $ do
        themeToggleButton
        logoutButton

    div_ [class_ "content wrapper-item"] $ do
      div_ [class_ "messages", data_ "simplebar" ""] $ do
        createMessage "my-message" "Гэндальф" "Волшебник, значит, не опаздывает, он просто приходит тогда, когда считает нужным." "20:46"
        createMessage "others-message" "Фродо" "Навигатором ты будешь, Гэндальф?" "20:50"
        createMessage "my-message" "Гэндальф" "Да, Фродо, только присяду - и сразу вперёд погнали!" "20:55"
        createMessage "others-message" "Фродо" "Смотри, кто-то топает! Арагорн, ты?" "21:00"
        createMessage "others-message" "Арагорн" "Угадал, мелкий! Испугался, что ли?" "21:05"
        createMessage "my-message" "Гэндальф" "Всё как обычно, малыш нервничает. Держись, Арагорн, нам дальше идти." "21:10"
        createMessage "others-message" "Фродо" "Ну не совсем испугался, но тут темнота какая-то неприятная." "21:15"
        createMessage "others-message" "Арагорн" "Не волнуйся, Фродо. Пока я с вами, мы в безопасности." "21:20"
        createMessage "my-message" "Гэндальф" "Арагорн прав. Главное, не терять бдительность, но и паниковать не стоит." "21:25"
        createMessage "others-message" "Фродо" "Давайте сделаем привал через пару миль. Нужно обсудить план." "21:30"
        createMessage "others-message" "Арагорн" "Согласен, недалеко отсюда есть хорошее место, где можно отдохнуть." "21:35"
        createMessage "my-message" "Гэндальф" "Тогда вперёд. Поговорим на привале. Запасайтесь силой, пригодится." "21:40"
      div_ [class_ "message-input-wrapper"] $
        fieldset_ [role_ "group", class_ "mb-0"] $ do
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
          button_ [class_ "outline send-message", handleSendMessageOnClick] $ makeIcon PaperAirplane
    div_ [class_ "members-header wrapper-item header-color"] "Members"
    div_ [class_ "members wrapper-item"] $ do
      createMember "Гэндальф"
      createMember "Арагорн"
      createMember "Фродо"

createMember :: Text -> Html ()
createMember username = p_ [class_ $ nameToColorClass username] $ small_ $ toHtml username

colorNames :: [Text]
colorNames =
  [ "red",
    "pink",
    "fuchsia",
    "purple",
    "violet",
    "indigo",
    "blue",
    "azure",
    "cyan",
    "jade",
    "green",
    "lime",
    "yellow",
    "amber",
    "pumpkin",
    "orange",
    "sand",
    "grey",
    "zinc",
    "slate"
  ]

nameToColorClass :: Text -> Text
nameToColorClass t =
  "username-" <> color
  where
    color = colorNames !! (hash t `mod` length colorNames)

createMessage :: Text -> Text -> Html () -> Html () -> Html ()
createMessage messageClass author message time =
  div_ [class_ ("message " <> messageClass)] $ do
    div_ [class_ "message-header"] $ do
      div_ [class_ $ nameToColorClass author] $ strong_ $ small_ $ toHtml author
      div_ $ small_ time
    div_ [class_ "message-content"] $ do
      small_ message

shorten :: Int -> Text -> Text
shorten n t =
  if Text.length t > n
    then Text.take m t <> "..." <> Text.takeEnd m t
    else t
  where
    m = n `div` 2

handleChatSelect :: Attribute
handleChatSelect = hyper_ "on click set global chat to @data-value"

checkLogin :: Attribute
checkLogin =
  hyper_
    [qc|
on load
if not localStorage.user
go to url '/login'
|]

autoresizeMessageInput :: String
autoresizeMessageInput =
  [qc|
js autoResize(document.getElementById('message-input')) end
|]

postMessageTemplate :: Text -> String
postMessageTemplate message =
  [qc|
fetch /message with
\{
  method:'POST',
  headers: \{'Content-Type': 'application/json'},
  body: \{
    message: {message},
    author: localStorage.user,
    chat: chat
  }
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
