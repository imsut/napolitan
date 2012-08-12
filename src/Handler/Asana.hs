{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Asana(getAsanaR
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
