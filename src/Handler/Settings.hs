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

data Settings = Settings { apiKey :: Text } deriving Show

settingsAForm :: Maybe Settings -> AForm App App Settings
settingsAForm mset = Settings
    <$> areq textField "Asana API Key" (apiKey <$> mset)

settingsForm :: Maybe AsanaConfig -> Html
                -> MForm App App (FormResult Settings, Widget)
settingsForm Nothing = renderTable $ settingsAForm Nothing
settingsForm (Just (AsanaConfig _ key _)) =
  renderTable $ settingsAForm (Just (Settings key))

getSettingsR :: Handler RepHtml
getSettingsR = do
    aid <- fromJust <$> maybeAuthId
    mrec <- runDB $ getBy $ UniqueConfigByUserId aid
    let mconfig = entityVal <$> mrec
    (widget, enctype) <- generateFormPost $ settingsForm mconfig
    defaultLayout $ do
      setTitle "Settings - Napolitan"
      $(widgetFile "settings")
      $(widgetFile "pomodoro-js")

postSettingsR :: Handler RepHtml
postSettingsR = do
  ((result, _), _) <- runFormPost $ settingsForm Nothing
  liftIO $ debugLog $ show result
  aid <- fromJust <$> maybeAuthId
  case result of
    FormSuccess (Settings key) -> do
      wks <- liftIO $ A.getWorkspaces key
      case wks of
        [] -> redirect SettingsR -- invalid key; show form again
        _ -> do
          runDB $ do
            mrec <- getBy $ UniqueConfigByUserId aid
            case mrec of
              Nothing -> insert $ AsanaConfig aid key $ fmap A.persist wks
              Just (Entity eid _) -> do
                update eid [ AsanaConfigApiKey =. key, AsanaConfigWorkspaces =. fmap A.persist wks ]
                return eid
      redirect HomeR
    _ -> redirect SettingsR -- show form again

--        aDomId <- lift newIdent
--        <label for=#{aDomId}>Asana API Key
--        <input type="text" id=#{aDomId} class="span3" placeholder="Type something…">
