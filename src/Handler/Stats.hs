{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Stats where

import Import

import Data.Aeson (toJSON)
import Data.Text (unpack)
import Data.Time.Calendar (Day)
import qualified Data.Time.Format as F
import qualified Data.Time.LocalTime as L
import DebugUtil
import System.Locale (defaultTimeLocale)

getStatsR :: Text -> Handler RepHtml
getStatsR _ = do
  redirect HomeR

getDailyStatsR :: Text -> Text -> Handler RepJson
getDailyStatsR screenName dateString = do
    day <- liftIO $ case mday of
      Nothing -> today
      Just d -> return d
    tasks <- runDB $ do
      Just (Entity uid _) <- getBy $ UniqueName screenName
      selectList [ FinishedTaskUserId ==. uid, FinishedTaskFinishOn ==. day ] []
    liftIO $ debugLog $ show $ toJSON $ fmap entityVal tasks
    jsonToRepJson $ fmap entityVal tasks
  where
    mday = F.parseTime defaultTimeLocale "%Y-%m-%d" $ unpack dateString :: Maybe Day
    today = fmap L.localDay $ fmap L.zonedTimeToLocalTime L.getZonedTime
