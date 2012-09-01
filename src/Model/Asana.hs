{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances, ImplicitPrelude #-}
module Model.Asana (Workspace(..)
                   , Task(..)
                   , getWorkspaces
                   , getTasks
                   , completeTask
                   , interruptTask
                   ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as S
import Data.Maybe
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as E
import Database.Persist
import Database.Persist.Store
import qualified Network.HTTP.Conduit as C

urlWorkspaces :: String
urlWorkspaces = "https://app.asana.com/api/1.0/workspaces"

-- e.g. https://app.asana.com/api/1.0/workspaces/723538443138/tasks\?assignee=me\&opt_fields=assignee_status,name,due_on,projects,projects.name
urlTasks :: Text -> String
urlTasks workspaceId = "https://app.asana.com/api/1.0/workspaces/"
  ++ unpack workspaceId
  ++ "/tasks?assignee=me&opt_fields=assignee_status,name,due_on,projects,projects.name,completed"

urlTask :: Text -> String
urlTask t = "https://app.asana.com/api/1.0/tasks/" ++ unpack t

data Workspace = Workspace { ident :: Text
                           , name :: Text
                           } deriving Show

instance PersistField Workspace where
  toPersistValue (Workspace i n) = PersistMap [("id", PersistText i), ("name", PersistText n)]
  fromPersistValue (PersistMap lst) = case (lookup "id" lst, lookup "name" lst) of
    (Just (PersistText i), Just (PersistText n)) -> Right $ Workspace i n
    _ -> Left $ pack ("failed to parse " ++ show lst)
  fromPersistValue (PersistList ((PersistText i):(PersistText n):[])) = Right $ Workspace i n
  sqlType _ = SqlString

-- parse JSON and construct a list of AsanaWorkspace
-- {"data":[{"id":42742032946,"name":"RevenueEng"},{"id":723538443138,"name":"Family / Personal"}]}
instance FromJSON [Workspace] where
  parseJSON (Object v) = do
    list <- v .: "data"
    mapM parse list
    where
      parse :: Object -> Parser Workspace
      parse x = Workspace <$> (pack <$> (show <$> (x .: "id" :: Parser Integer))) <*> x .: "name"
  parseJSON _ = return []

instance ToJSON Workspace where
  toJSON w = object $ [ "id" .= ident w, "name" .= name w ]

data Task = Task { taskId :: Text
                 , taskName :: Text
                 , taskStatus :: Text
                 , projectName :: Maybe Text
                 , dueOn :: Maybe Text
                 , completed :: Bool
                 } deriving Show

instance FromJSON [Task] where
  parseJSON (Object v) = do
      list <- v .: "data"
      mapM parse list
    where
      parse :: Object -> Parser Task
      parse x = Task <$> (pack <$> (show <$> (x .: "id" :: Parser Integer)))
                     <*> x .: "name"
                     <*> x .: "assignee_status"
                     <*> projectName x
                     <*> x .:? "due_on"
                     <*> x .: "completed"
      projectName :: Object -> Parser (Maybe Text)
      projectName y = do
        projects <- y .: "projects"
        case projects of
          [] -> return Nothing
          (p:_) -> fmap Just (p .: "name")
  parseJSON _ = return []

instance ToJSON Task where
  toJSON t = object $ catMaybes [ Just $ "id" .= taskId t
                                , Just $ "name" .= taskName t
                                , Just $ "status" .= taskStatus t
                                , ("projectName" .=) <$> projectName t
                                , Just $ "due_on" .= dueOn t
                                ]

getWorkspaces :: Text -> IO [Workspace]
getWorkspaces key = do
    resp <- C.withManager $ \m -> C.httpLbs (makeRequest key urlWorkspaces) m
    case decode $ C.responseBody resp of
      Nothing -> return []
      Just workspaces -> return workspaces

getTasks :: Text -> Text -> IO [Task]
getTasks key workspaceId = do
    resp <- C.withManager $ \m -> C.httpLbs (makeRequest key $ urlTasks workspaceId) m
    case decode $ C.responseBody resp of
      Nothing -> return []
      Just tasks -> return tasks

completeTask :: Text -> Text -> IO ()
completeTask = updateTask [("completed", "true")]

interruptTask :: Text -> Text -> IO ()
interruptTask = updateTask [("assignee_status", "upcoming")]

updateTask :: [(S.ByteString, S.ByteString)] -> Text -> Text -> IO ()
updateTask params key taskId = do
    _ <- C.withManager $ \m -> C.httpLbs req m
    return ()
  where
    req = (C.urlEncodedBody params (makeRequest key $ urlTask taskId)) { C.method = "PUT" }

makeRequest :: Text -> String -> C.Request m
makeRequest key url = (C.applyBasicAuth (E.encodeUtf8 key) "" . fromJust . C.parseUrl) $ url
