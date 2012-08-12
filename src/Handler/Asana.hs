{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Asana(getAsanaR
                    ) where

import Import
import qualified Data.Map as M
import qualified Model.Asana as A
import Network.HTTP.Types

getAsanaR :: Text -> Handler RepJson
getAsanaR "workspaces" = do
    mkey <- lookupGetParam "key"
    case mkey of
      Nothing -> jsonToRepJson () >>= sendResponseStatus badRequest400
      Just "" -> jsonToRepJson () >>= sendResponseStatus badRequest400
      Just key -> do
        wks <- liftIO $ A.getWorkspaces key
        jsonToRepJson $ M.fromList $ fmap (\a -> (A.name a, A.ident a)) wks
getAsanaR _ = jsonToRepJson () >>= sendResponseStatus notFound404
