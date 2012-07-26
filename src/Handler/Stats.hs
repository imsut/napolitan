{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Stats where

import Import

getStatsR :: Text -> Handler RepHtml
getStatsR _ = do
  redirect HomeR
