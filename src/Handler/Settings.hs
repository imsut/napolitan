{-# LANGUAGE TupleSections, OverloadedStrings, FlexibleInstances #-}
module Handler.Settings where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Data.Text
import qualified Data.Text.Encoding as E
import Import
import Model.Asana
import Network.HTTP.Conduit as C
import Yesod.Auth (maybeAuthId)

data Settings = Settings
                { settingsAsanaKey :: Text
                , settingsAsanaWsId :: [Int]
                }
                deriving Show

settingsAForm :: Maybe Settings -> [AsanaWorkspace] -> AForm App App Settings
settingsAForm mset wks = Settings
    <$> areq textField keySettings (settingsAsanaKey <$> mset)
    <*> areq (multiSelectFieldList wklist) "Workspace" (settingsAsanaWsId <$> mset)
  where
    wklist = Import.map (\(AsanaWorkspace ident name) -> (name, ident)) wks
    keySettings = FieldSettings {
        fsLabel = fromString "Asana API Key"
      , fsTooltip = Nothing
      , fsId = Just "fieldAsanaKey"
      , fsName = Nothing
      , fsAttrs = []
      }

settingsForm :: Maybe AsanaConfig -> [AsanaWorkspace] -> Html
                -> MForm App App (FormResult Settings, Widget)
settingsForm Nothing wks = renderTable $ settingsAForm Nothing wks
settingsForm (Just (AsanaConfig _ key selected)) wks =
  renderTable $ settingsAForm (Just (Settings key selected)) wks

--asanaKeyForm :: Maybe Text -> Form Text
--asanaKeyForm key = renderDivs $ areq textField "Asana API Key" key

getSettingsR :: Handler RepHtml
getSettingsR = do
    aid <- fromJust <$> maybeAuthId
    mrec <- runDB $ getBy $ UniqueConfigByUserId aid
    let mconfig = entityVal <$> mrec
    workspaces <- case mconfig of
      Nothing -> return []
      Just config -> liftIO $ getAsanaWorkspaces $ asanaConfigApiKey config
    (widget, enctype) <- generateFormPost $ settingsForm mconfig workspaces
    defaultLayout $ do
      setTitle "Settings - Napolitan"
      $(widgetFile "settings")
      $(widgetFile "pomodoro-js")

  -- mauth <- maybeAuth
  -- case mauth of
  --   Nothing -> redirect HomeR
  --   Just (Entity _ user) -> do
  --     let masanaKey = (userAsanaApiKey user) :: Maybe Text
  --         masanaWksId = (userAsanaWorkspaceId user) :: Maybe Int
  --     workspaces <- liftIO $ getAsanaWorkspaces masanaKey
  --     (widget, enctype) <- generateFormPost $ settingsForm masanaKey masanaWksId workspaces
  --     defaultLayout $ do
  --       setTitle "Settings - Napolitan"
  --       $(widgetFile "settings")

postSettingsR :: Handler RepHtml
postSettingsR = do
  ((result, _), _) <- runFormPost $ settingsForm Nothing []
  aid <- fromJust <$> maybeAuthId
  case result of
    FormSuccess (Settings key workspaces) -> do
      runDB $ do
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
