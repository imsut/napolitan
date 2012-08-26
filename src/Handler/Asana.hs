{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Asana(getWorkspacesR
                    , getTasksR
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

-- not used now.
-- assumes AsanaConfig table has a record for this user
getSyncR :: Handler RepJson
getSyncR = do
    aid <- fromJust <$> maybeAuthId
    mwkid <- lookupGetParam "workspace"
    case mwkid of
      Nothing -> return ()
      Just wkid -> setSession "workspaceId" wkid
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
    let json = toJSON (
          wks,
          filter (\t -> (not $ A.completed t) && (A.taskStatus t == "today")) tasks
          )
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

-- assumes AsanaConfig table has a record for this user
getWorkspacesR :: Handler RepJson
getWorkspacesR = do
    aid <- fromJust <$> maybeAuthId
    mkey <- do
      mrec <- runDB $ getBy $ UniqueConfigByUserId aid
      (return . Just . asanaConfigApiKey . entityVal . fromJust) mrec
    wks <- case mkey of
      Nothing -> return []
      Just key -> liftIO $ A.getWorkspaces key
    runDB $ do
      mrec <- getBy $ UniqueConfigByUserId aid
      case mrec of
        Nothing -> return ()
        Just (Entity eid _) ->
          update eid [ AsanaConfigWorkspaces =. fmap A.persist wks ]
    let json = toJSON wks
    jsonToRepJson json

-- assumes AsanaConfig table has a record for this user
getTasksR :: Text -> Handler RepJson
getTasksR workspaceId = do
    aid <- fromJust <$> maybeAuthId
    setSession "workspaceId" workspaceId
    mkey <- do
      mrec <- runDB $ getBy $ UniqueConfigByUserId aid
      (return . Just . asanaConfigApiKey . entityVal . fromJust) mrec
    tasks <- case mkey of
      Nothing -> return []
      Just key -> liftIO $ A.getTasks key workspaceId
    let json = toJSON $ filter (\t -> (not $ A.completed t) && (A.taskStatus t == "today")) tasks
    jsonToRepJson json
