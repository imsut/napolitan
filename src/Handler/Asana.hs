{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Asana where

import Import
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text.Encoding as E
import qualified Model.Asana as A
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types
import Yesod.Auth
import Yesod.Json

getAsanaR :: Text -> Handler RepJson
getAsanaR "workspaces" = do
    mkey <- lookupGetParam "key"
    case mkey of
      Nothing -> jsonToRepJson () >>= sendResponseStatus badRequest400
      Just "" -> jsonToRepJson () >>= sendResponseStatus badRequest400
      Just key -> do
        jsonToRepJson $ M.fromList ([("foo", 123), ("bar", 456)] :: [(String, Integer)])
getAsanaR _ = jsonToRepJson () >>= sendResponseStatus notFound404
