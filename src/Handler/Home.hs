{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home (getHomeR
                    , postPomodoroR
                    , postBreakR
                    , postTaskR) where

import Import
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (unpack)
import Data.Time
import DebugUtil
import System.Locale (defaultTimeLocale)
import Yesod.Auth
import qualified Yesod.Auth.OAuth as OA

import Model.Asana

textToUTCTime :: UTCTime -> Text -> UTCTime
textToUTCTime currentTime text =
    fromMaybe currentTime
    $ Data.Time.parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ unpack text

pomodoroForm :: UTCTime -> FormInput App App Pomodoro
pomodoroForm currentTime = Pomodoro
      <$> ireq dayField "startOn"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "startAt"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "endAt"
      <*> ireq textField "taskId"
      <*> ireq textField "taskName"

breakForm :: UTCTime -> FormInput App App Break
breakForm currentTime = Break
      <$> ireq dayField "startOn"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "startAt"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "endAt"

taskForm :: UTCTime -> FormInput App App FinishedTask
taskForm currentTime = FinishedTask
      <$> ireq dayField "finishOn"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "finishAt"
      <*> ireq textField "ident"
      <*> ireq textField "name"
      <*> ireq textField "status"

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

postPomodoroR :: Handler RepJson
postPomodoroR = do
  utcTime <- liftIO $ getCurrentTime
  pomodoro <- runInputPost $ pomodoroForm utcTime
  liftIO $ debugLog $ show pomodoro
  _ <- runDB $ insert pomodoro
  jsonToRepJson ()

postBreakR :: Handler RepJson
postBreakR = do
  utcTime <- liftIO $ getCurrentTime
  break <- runInputPost $ breakForm utcTime
  liftIO $ debugLog $ show break
  _ <- runDB $ insert break
  jsonToRepJson ()

postTaskR :: Handler RepJson
postTaskR = do
  aid <- fromJust <$> maybeAuthId
  utcTime <- liftIO $ getCurrentTime
  task <- runInputPost $ taskForm utcTime
  liftIO $ debugLog $ show task
  mrec <- runDB $ getBy $ UniqueConfigByUserId aid
  case mrec of
    Nothing -> redirect SettingsR -- need to set Asana API key
    Just (Entity _ (AsanaConfig _ key _)) -> do
      _ <- runDB $ insert task
      liftIO $ case task of
        FinishedTask _ _ taskId _ "Complete" -> completeTask key taskId
        FinishedTask _ _ taskId _ "Continue" -> interruptTask key taskId
        _ -> return () -- should return 300
      jsonToRepJson ()
