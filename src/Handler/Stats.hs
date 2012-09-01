{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Stats where

import Import

import Data.Maybe
import Data.Text (unpack)
import Data.Time.Calendar (Day)
import qualified Data.Time.Format as F
import qualified Data.Time.LocalTime as L
import System.Locale (defaultTimeLocale)

getStatsR :: Text -> Handler RepHtml
getStatsR _ = do
  redirect HomeR

getDailyStatsR :: Text -> Text -> Handler RepJson
getDailyStatsR screenName dateString = do
    -- works, but ugly
    day <- liftIO $ fmap (flip fromMaybe mday) today
    tasks <- runDB $ do
      Just (Entity uid _) <- getBy $ UniqueName screenName
      selectList [ FinishedTaskUserId ==. uid, FinishedTaskFinishOn ==. day ] []
    jsonToRepJson $ fmap entityVal tasks
  where
    mday = F.parseTime defaultTimeLocale "%Y-%m-%d" $ unpack dateString :: Maybe Day
    today = fmap L.localDay $ fmap L.zonedTimeToLocalTime L.getZonedTime
