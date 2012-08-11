{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import qualified Yesod.Auth.OAuth as OA

getHomeR :: Handler RepHtml
getHomeR = do
  maid <- maybeAuthId
  case maid of
    Just _ -> do
      -- add something here
      defaultLayout $ do
        setTitle "Pomodoro - Napolitan"
        $(widgetFile "pomodoro-js")
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
