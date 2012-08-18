{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Asana(getAsanaR,
                     getSyncR
                    ) where

import Import
import Data.Aeson (toJSON)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (unpack)
import Data.Time.Calendar (Day)
import qualified Data.Time.Format as F
import qualified Model.Asana as A
import Network.HTTP.Types
import System.Locale (defaultTimeLocale)
import Yesod.Auth (maybeAuthId)

import DebugUtil

getAsanaR :: Text -> Handler RepJson
getAsanaR "workspaces" = do
    aid <- fromJust <$> maybeAuthId
    mkey <- lookupGetParam "key"
    case mkey of
      Nothing -> jsonToRepJson () >>= sendResponseStatus badRequest400
      Just "" -> jsonToRepJson () >>= sendResponseStatus badRequest400
      Just key -> do
        wks <- liftIO $ A.getWorkspaces key
        liftIO $ do
          debugLog $ show key
          debugLog $ show wks
        jsonToRepJson $ M.fromList $ fmap (\a -> (A.ident a, A.name a)) wks
getAsanaR _ = jsonToRepJson () >>= sendResponseStatus notFound404

-- only supports Asana now
-- assumes AsanaConfig table has a record for this user
getSyncR :: Handler RepJson
getSyncR = do
    aid <- fromJust <$> maybeAuthId
    mwkid <- lookupGetParam "workspace"
    mkey <- do
      mrec <- runDB $ getBy $ UniqueConfigByUserId aid
      (return . Just . asanaConfigApiKey . entityVal . fromJust) mrec
    wks <- case mkey of
      Nothing -> return []
      Just key -> liftIO $ A.getWorkspaces key
    tasks <- case (mwkid, mkey) of
      (Nothing, _) -> return []
      (Just "", _) -> return []
      (_, Nothing) -> return []
      (Just wkid, Just key) -> liftIO $ A.getTasks key wkid
    -- runDB $ do
    --   mrec <- getBy $ UniqueConfigByUserId aid
    --   case mrec of
    --     Nothing -> return ()
    --     Just (Entity eid _) ->
    --       update eid [ AsanaConfigWorkspaces =. fmap A.persist wks ]
    --   mapM_ updateTask tasks
    let json = toJSON (wks, A.filterByStatus "today" tasks)
    jsonToRepJson json
  where
    updateTask t = do
      mrec <- getBy $ UniqueTaskByExtId $ A.taskId t
      case mrec of
        Nothing -> insert $ Task (A.taskId t) (A.taskName t) (A.dueOn t >>= textToDay)
        Just (Entity eid _) -> do
          update eid [ TaskName =. (A.taskName t)
                     , TaskDueOn =. (A.dueOn t >>= textToDay) ]
          return eid

    textToDay :: Text -> Maybe Day
    textToDay = F.parseTime defaultTimeLocale "%Y-%m-%d" . unpack
