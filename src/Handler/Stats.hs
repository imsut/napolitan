{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Stats
       (
         getStatsR
       , getDataR
       )where

import Import

import qualified Data.Map as M
import Data.Maybe
import Data.Text (unpack)
import Data.Time.Calendar (Day)
import qualified Data.Time.Format as F
import qualified Data.Time.LocalTime as L
import System.Locale (defaultTimeLocale)

import DebugUtil

getStatsR :: Text -> Handler RepHtml
getStatsR _ = do
  defaultLayout $ do
    setTitle "Your Achievements - Napolitan"
    addScript $ StaticR js_highcharts_js
    $(widgetFile "stats")
    $(widgetFile "stats-js")

getDataR :: Text -> Text -> Handler RepJson
getDataR screenName "task" = getTaskData screenName
getDataR screenName "pomodoro" = getPomodoroData screenName
getDataR _ _ = notFound

getTaskData :: Text -> Handler RepJson
getTaskData screenName = do
    mon <- lookupGetParam "on"
    case mon of
      Nothing -> do
        tasks <- runDB $ do
          Just (Entity uid _) <- getBy $ UniqueName screenName
          selectList [ FinishedTaskUserId ==. uid ] []
        jsonToRepJson $ fmap entityVal tasks
      Just on -> do
        -- works, but ugly
        day <- liftIO $ fmap (flip fromMaybe $ mday on) today
        tasks <- runDB $ do
          Just (Entity uid _) <- getBy $ UniqueName screenName
          selectList [ FinishedTaskUserId ==. uid, FinishedTaskFinishOn ==. day ] []
        jsonToRepJson $ fmap entityVal tasks
  where
    mday :: Text -> Maybe Day
    mday = (F.parseTime defaultTimeLocale "%Y-%m-%d") . unpack
    today = fmap L.localDay $ fmap L.zonedTimeToLocalTime L.getZonedTime

getPomodoroData :: Text -> Handler RepJson
getPomodoroData screenName = do
    mon <- lookupGetParam "on"
    records <- case mon of
      Nothing -> runDB $ do
        Just (Entity uid _) <- getBy $ UniqueName screenName
        selectList [ PomodoroUserId ==. uid ] []
      Just on -> do
        -- works, but ugly
        day <- liftIO $ fmap (flip fromMaybe $ mday on) today
        runDB $ do
          Just (Entity uid _) <- getBy $ UniqueName screenName
          selectList [ PomodoroUserId ==. uid, PomodoroStartOn ==. day ] []
    let pomodoros = fmap entityVal records
        pairList = fmap (\p@(Pomodoro _ d _ _ _ _) -> (fmt d, [p])) pomodoros
        pomodoroMap = foldr (\(d, l) m -> M.insertWith (++) d l m) M.empty pairList
    liftIO $ debugLog $ show pomodoroMap
    jsonToRepJson pomodoroMap
  where
    mday :: Text -> Maybe Day
    mday = (F.parseTime defaultTimeLocale "%Y-%m-%d") . unpack
    today = fmap L.localDay $ fmap L.zonedTimeToLocalTime L.getZonedTime
    fmt = F.formatTime defaultTimeLocale "%Y-%m-%d"
