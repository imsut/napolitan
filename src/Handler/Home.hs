{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Maybe (fromMaybe)
import Yesod.Auth
import qualified Yesod.Auth.OAuth as OA

import Model.Asana

getHomeR :: Handler RepHtml
getHomeR = do
  maid <- maybeAuthId
  case maid of
    Just aid -> do
      mrec <- runDB $ getBy $ UniqueConfigByUserId aid
      case mrec of
        Nothing -> redirect SettingsR -- need to set Asana API key
        Just (Entity _ (AsanaConfig _ _ wks)) -> do
          mworkspace <- lookupSession "workspaceId"
          let workspaces = fmap unpersist wks
              selectedWorkspace = fromMaybe "" mworkspace
          defaultLayout $ do
            setTitle "Pomodoro - Napolitan"
            $(widgetFile "pomodoro-js")
            $(widgetFile "pomodoro")
    Nothing -> defaultLayout $ do
      setTitle "Napolitan = Asana + Pomorodo"
      $(widgetFile "welcome")
