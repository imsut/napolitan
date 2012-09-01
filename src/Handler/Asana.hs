{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Asana(getWorkspacesR
                    , getTasksR
                    ) where

import Import
import Data.Aeson (toJSON)
import Data.Maybe
import qualified Model.Asana as A
import Yesod.Auth (maybeAuthId)

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
          update eid [ AsanaConfigWorkspaces =. wks ]
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
