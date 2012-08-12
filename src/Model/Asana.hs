{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances, ImplicitPrelude #-}
module Model.Asana where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import qualified Network.HTTP.Conduit as C

urlAsanaWorkspaces :: String
urlAsanaWorkspaces = "https://app.asana.com/api/1.0/workspaces"

data Workspace = Workspace { ident :: Int,
                                       name :: Text
                                     } deriving Show

-- parse JSON and construct a list of AsanaWorkspace
-- {"data":[{"id":42742032946,"name":"RevenueEng"},{"id":723538443138,"name":"Family / Personal"}]}
instance FromJSON [Workspace] where
  parseJSON (Object v) = do
    list <- v .: "data"
    parseJSON' list
    where
      parseJSON' :: [Object] -> Parser [Workspace]
      parseJSON' = mapM $ \x -> Workspace <$> x .: "id" <*> x .: "name"
  parseJSON _ = return []

getWorkspaces :: Text -> IO [Workspace]
getWorkspaces key = do
  resp <- C.withManager $ \m ->
    C.httpLbs req m
  case decode $ C.responseBody resp of
    Nothing -> return []
    Just workspaces -> return workspaces
  where
    req = C.applyBasicAuth (E.encodeUtf8 key) "" $ fromJust $ C.parseUrl urlAsanaWorkspaces
