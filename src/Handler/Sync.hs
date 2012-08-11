{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Sync where

import Import
import qualified Data.Map as M
import Yesod.Auth
import Yesod.Json

-- only supports Asana now
getSyncR :: Text -> Handler RepJson
getSyncR _ = do
    maid <- maybeAuthId
    case maid of
      Nothing -> jsonToRepJson ()
      Just aid -> do
        mrec <- runDB $ getBy $ UniqueConfigByUserId aid
        case mrec of
          Nothing -> jsonToRepJson()
          Just (Entity _ (AsanaConfig _ key workspaces)) ->
            jsonToRepJson $ M.fromList ([("foo", True), ("bar", False)] :: [(String, Bool)])
