{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import qualified Yesod.Auth.OAuth as OA

getHomeR :: Handler RepHtml
getHomeR = do
  maid <- maybeAuthId
  case maid of
    Just _ -> defaultLayout $ do
      setTitle "Pomodoro - Napolitan"
      $(widgetFile "pomodoro")
    Nothing -> defaultLayout $ do
      setTitle "Napolitan = Asana + Pomorodo"
      $(widgetFile "welcome")
    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    -- let submission = Nothing :: Maybe (FileInfo, Text)
    --     handlerName = "getHomeR" :: Text
    -- defaultLayout $ do
    --     aDomId <- lift newIdent
    --     setTitle "Welcome To Yesod!"
    --     $(widgetFile "homepage")

-- sampleForm :: Form (FileInfo, Text)
-- sampleForm = renderDivs $ (,)
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField "What's on the file?" Nothing
