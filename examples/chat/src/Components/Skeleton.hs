module Components.Skeleton where

import Lucid

messageSkeleton :: (Monad m) => HtmlT m ()
messageSkeleton = div_ [class_ "message-skeleton animate-pulse", role_ "status"] $ do
  div_ [class_ "message-header"] $ do
    div_ [class_ "message-skeleton-item author"] ""
    div_ [class_ "message-skeleton-item datetime"] ""
  div_ [class_ "message-skeleton-item message"] ""
  span_ [class_ "sr-only"] "Loading..."
