module Pages.Main (mainPage) where

import Components.Head
import Components.Icons
import Components.LogoutButton
import Components.ThemeToggleButton
import Data.Hashable (hash)
import Data.Text (Text)
import Lucid
import Monad
import Utils.Attributes
import Web.Scotty.Trans

mainPage :: ActionT AppM ()
mainPage = do
  html $ renderText $ do
    doctype_
    html_ [lang_ "en"] $ do
      htmlHead
      htmlBody

htmlBody :: Html ()
htmlBody = body_ [class_ "h-screen"] $ do
  div_ [class_ "wrapper"] $ do
    div_ [class_ "sidebar-header wrapper-item header-color"] "Page name"
    div_ [class_ "sidebar wrapper-item "] $ do
      p_ [class_ "header-color"] "Chats"
      p_ $ small_ "Братаны из Братства"
      p_ $ small_ "Шпионский расчёт в Шире"
      p_ $ small_ "Орки и прочие дикие"
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
          textarea_ [class_ "message-input", name_ "input", placeholder_ "Message", ariaLabel_ "Message", rows_ "1", oninput_ "autoResize(this)"] ""
          button_ [class_ "outline send-message"] $ makeIcon PaperAirplane
    div_ [class_ "members-header wrapper-item header-color"] "Members"
    div_ [class_ "members wrapper-item"] $ do
      createMember "Гэндальф"
      createMember "Арагорн"
      createMember "Фродо"

createMember :: Text -> Html ()
createMember username = p_ [class_ $ nameToColorClass username] $ small_ $ toHtml username

colorNames :: [Text]
colorNames =
  [ "red"
  , "pink"
  , "fuchsia"
  , "purple"
  , "violet"
  , "indigo"
  , "blue"
  , "azure"
  , "cyan"
  , "jade"
  , "green"
  , "lime"
  , "yellow"
  , "amber"
  , "pumpkin"
  , "orange"
  , "sand"
  , "grey"
  , "zinc"
  , "slate"
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