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

pomodoroForm :: UserId -> UTCTime -> FormInput App App Pomodoro
pomodoroForm userId currentTime = Pomodoro userId
      <$> ireq dayField "startOn"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "startAt"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "endAt"
      <*> iopt textField "taskId"
      <*> iopt textField "taskName"

breakForm :: UserId -> UTCTime -> FormInput App App Break
breakForm userId currentTime = Break userId
      <$> ireq dayField "startOn"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "startAt"
      <*> (textToUTCTime currentTime) `fmap` ireq textField "endAt"

taskForm :: UserId -> UTCTime -> FormInput App App FinishedTask
taskForm userId currentTime = FinishedTask userId
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
        Just (Entity _ (AsanaConfig _ _ workspaces)) -> do
          mscreenName <- lookupSession "screenName"
          mworkspace <- lookupSession "workspaceId"
          let selectedWorkspace = fromMaybe "" mworkspace
          defaultLayout $ do
            setTitle "Pomodoro - Napolitan"
            $(widgetFile "pomodoro")
            $(widgetFile "pomodoro-js")
    Nothing -> defaultLayout $ do
      setTitle "Napolitan = Asana + Pomodoro"
      $(widgetFile "welcome")

postPomodoroR :: Handler RepJson
postPomodoroR = do
  aid <- fromJust <$> maybeAuthId
  utcTime <- liftIO $ getCurrentTime
  pomodoro <- runInputPost $ pomodoroForm aid utcTime
  liftIO $ debugLog $ show pomodoro
  _ <- runDB $ insert pomodoro
  jsonToRepJson ()

postBreakR :: Handler RepJson
postBreakR = do
  aid <- fromJust <$> maybeAuthId
  utcTime <- liftIO $ getCurrentTime
  break <- runInputPost $ breakForm aid utcTime
  liftIO $ debugLog $ show break
  _ <- runDB $ insert break
  jsonToRepJson ()

postTaskR :: Handler RepJson
postTaskR = do
  aid <- fromJust <$> maybeAuthId
  utcTime <- liftIO $ getCurrentTime
  task <- runInputPost $ taskForm aid utcTime
  liftIO $ debugLog $ show task
  mrec <- runDB $ getBy $ UniqueConfigByUserId aid
  case mrec of
    Nothing -> redirect SettingsR -- need to set Asana API key
    Just (Entity _ (AsanaConfig _ key _)) -> do
      _ <- runDB $ insert task
      liftIO $ case task of
        FinishedTask _ _ _ taskId _ "Complete" -> completeTask key taskId
        FinishedTask _ _ _ taskId _ "Continue" -> interruptTask key taskId
        _ -> return () -- should return 300
      jsonToRepJson ()
