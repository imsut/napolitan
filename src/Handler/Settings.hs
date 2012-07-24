{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Settings where

import Import
import Yesod.Auth (maybeAuthId, maybeAuth)
import Yesod.Logger

getSettingsR :: Handler RepHtml
getSettingsR = do
  mauth <- maybeAuth
  case mauth of
    Nothing -> redirect HomeR
    Just (Entity _ user) -> do
      let masanaKey = (userAsanaApiKey user) :: Maybe Text
      (widget, enctype) <- generateFormPost $ asanaKeyForm masanaKey
      defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Settings - Napolitan"
        $(widgetFile "settings")

postSettingsR :: Handler RepHtml
postSettingsR = do
  ((result, _), _) <- runFormPost $ asanaKeyForm Nothing
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect HomeR
    Just aid -> case result of
      FormSuccess asanaKey -> do
        runDB $ update aid [ UserAsanaApiKey =. Just asanaKey ]
        redirect HomeR
      _ -> redirect SettingsR -- show form again

asanaKeyForm :: Maybe Text -> Form Text
asanaKeyForm key = renderDivs $ areq textField "Asana API Key" key

--        <label for=#{aDomId}>Asana API Key
--        <input type="text" id=#{aDomId} class="span3" placeholder="Type something…">
