{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Asana(getAsanaR,
                     getSyncR
                    ) where

import Import
import qualified Data.Map as M
import Data.Maybe
import qualified Model.Asana as A
import Network.HTTP.Types
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

{-
asanaApiKey :: UserId -> GHandler s m (Maybe Text)
--asanaApiKey :: UserId -> YesodDB s m (Maybe Text)
asanaApiKey aid = runDB $ do
    (getBy $ UniqueConfigByUserId aid) >>= \mrec ->
      case mrec of
        Nothing -> return Nothing
        Just rec -> (return . Just . asanaConfigApiKey . entityVal) rec
-}

-- only supports Asana now
getSyncR :: Handler RepJson
getSyncR = do
    aid <- fromJust <$> maybeAuthId
    mwkid <- lookupGetParam "workspace"
    case mwkid of
      Nothing -> jsonToRepJson () >>= sendResponseStatus badRequest400
      Just "" -> jsonToRepJson () >>= sendResponseStatus badRequest400
      Just wkid -> do
        mkey <- do
          mrec <- runDB $ getBy $ UniqueConfigByUserId aid
          case mrec of
            Nothing -> return Nothing
            Just rec -> (return . Just . asanaConfigApiKey . entityVal) rec
        wks <- case mkey of
          Nothing -> return []
          Just key -> liftIO $ A.getWorkspaces key
        tasks <- case mkey of
          Nothing -> return []
          Just key -> liftIO $ A.getTasks key wkid
        jsonToRepJson $ (
            M.fromList ([("workspaces", True), ("bar", False)] :: [(Text, Bool)])
          , M.fromList ([("workspaces", True), ("bar", False)] :: [(Text, Bool)])
          )
