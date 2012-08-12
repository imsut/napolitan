{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Settings
    ( getSettingsR
    , postSettingsR
    ) where

import Data.Maybe
import Data.String
import Data.Text
import DebugUtil
import Import
import qualified Model.Asana as A
import Yesod.Auth (maybeAuthId)

data Settings = Settings { apiKey :: Text
                         , wksIds :: [Text]
                         } deriving Show

settingsAForm :: Maybe Settings -> [A.Workspace] -> AForm App App Settings
settingsAForm mset wks = Settings
    <$> areq textField keySettings (apiKey <$> mset)
    <*> areq (multiSelectFieldList wklist) wsSettings (wksIds <$> mset)
  where
    wklist = fmap (\(A.Workspace ident name) -> (name, ident)) wks
    keySettings = FieldSettings {
        fsLabel = fromString "Asana API Key"
      , fsTooltip = Nothing
      , fsId = Just "fieldAsanaKey"
      , fsName = Nothing
      , fsAttrs = []
      }
    wsSettings = FieldSettings {
        fsLabel = fromString "Asana Workspace"
      , fsTooltip = Nothing
      , fsId = Just "fieldAsanaWorkspaces"
      , fsName = Nothing
      , fsAttrs = []
      }

settingsForm :: Maybe AsanaConfig -> [A.Workspace] -> Html
                -> MForm App App (FormResult Settings, Widget)
settingsForm Nothing wks = renderTable $ settingsAForm Nothing wks
settingsForm (Just (AsanaConfig _ key selected)) wks =
  renderTable $ settingsAForm (Just (Settings key selected)) wks

getSettingsR :: Handler RepHtml
getSettingsR = do
    aid <- fromJust <$> maybeAuthId
    mrec <- runDB $ getBy $ UniqueConfigByUserId aid
    let mconfig = entityVal <$> mrec
    workspaces <- case mconfig of
      Nothing -> return []
      Just config -> liftIO $ A.getWorkspaces $ asanaConfigApiKey config
    (widget, enctype) <- generateFormPost $ settingsForm mconfig workspaces
    defaultLayout $ do
      setTitle "Settings - Napolitan"
      $(widgetFile "settings")
      $(widgetFile "pomodoro-js")

postSettingsR :: Handler RepHtml
postSettingsR = do
  ((result, _), _) <- runFormPost $ settingsForm Nothing []
  liftIO $ debugLog $ show result
  aid <- fromJust <$> maybeAuthId
  case result of
    FormSuccess (Settings key workspaces) -> do
      _ <- runDB $ do
        mrec <- getBy $ UniqueConfigByUserId aid
        case mrec of
          Nothing -> insert $ AsanaConfig aid key workspaces
          Just (Entity eid _) -> do
            update eid [ AsanaConfigApiKey =. key, AsanaConfigWorkspaces =. workspaces ]
            return eid
      redirect HomeR
    _ -> redirect SettingsR -- show form again

--        aDomId <- lift newIdent
--        <label for=#{aDomId}>Asana API Key
--        <input type="text" id=#{aDomId} class="span3" placeholder="Type something…">
