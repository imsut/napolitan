{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances, ImplicitPrelude #-}
module Model.Asana (Workspace(..),
                    PersistWorkspace,
                    Task(..),
                    persist,
                    unpersist,
                    getWorkspaces,
                    getTasks,
                   ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as E
import qualified Network.HTTP.Conduit as C

urlWorkspaces :: String
urlWorkspaces = "https://app.asana.com/api/1.0/workspaces"

-- e.g. https://app.asana.com/api/1.0/workspaces/723538443138/tasks\?assignee=me\&opt_fields=assignee_status,name,due_on,projects,projects.name
urlTasks :: Text -> String
urlTasks workspaceId = "https://app.asana.com/api/1.0/workspaces/"
  ++ unpack workspaceId
  ++ "/tasks?assignee=me&opt_fields=assignee_status,name,due_on,projects,projects.name"

data Workspace = Workspace { ident :: Text
                           , name :: Text
                           } deriving Show

type PersistWorkspace = (Text, Text)

-- parse JSON and construct a list of AsanaWorkspace
-- {"data":[{"id":42742032946,"name":"RevenueEng"},{"id":723538443138,"name":"Family / Personal"}]}
instance FromJSON [Workspace] where
  parseJSON (Object v) = do
    list <- v .: "data"
    parseJSON' list
    where
      parseJSON' :: [Object] -> Parser [Workspace]
      parseJSON' = mapM $ \x -> Workspace <$> (pack <$> (show <$> (x .: "id" :: Parser Integer))) <*> x .: "name"
  parseJSON _ = return []

data Task = Task { taskId :: Text
                 , taskName :: Text
                 , projectName :: Text
                 , dueOn :: Text
                 } deriving Show

instance FromJSON [Task] where
  parseJSON (Object v) = do
    undefined
  parseJSON _ = return []

persist :: Workspace -> PersistWorkspace
persist (Workspace ident name) = (ident, name)

unpersist :: PersistWorkspace -> Workspace
unpersist (ident, name) = Workspace ident name

getWorkspaces :: Text -> IO [Workspace]
getWorkspaces key = do
    resp <- C.withManager $ \m ->
      C.httpLbs req m
    case decode $ C.responseBody resp of
      Nothing -> return []
      Just workspaces -> return workspaces
  where
    req = C.applyBasicAuth (E.encodeUtf8 key) "" $ fromJust $ C.parseUrl urlWorkspaces

getTasks :: Text -> Text -> IO [Task]
getTasks key workspaceId = do
    resp <- C.withManager $ \m ->
      C.httpLbs req m
    case decode $ C.responseBody resp of
      Nothing -> return []
      Just workspaces -> return workspaces
  where
    req = C.applyBasicAuth (E.encodeUtf8 key) "" $ fromJust $ C.parseUrl $ urlTasks workspaceId
