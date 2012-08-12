{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Signup where

import DebugUtil
import Import
import qualified Yesod.Auth.OAuth as OA
import Yesod.Auth

getSignupR :: Handler RepHtml
getSignupR = do
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect $ AuthR OA.twitterUrl -- weird. try again
    Just aid -> do
      liftIO $ debugLog $ show maid
      mconfig <- runDB $ getBy $ UniqueConfigByUserId aid
      case mconfig of
        Just (Entity _ (AsanaConfig _ _ [])) -> redirect SettingsR -- workspace not set
        Just _ -> redirect HomeR
        Nothing -> redirect SettingsR -- record doesn't exist

-- case mauth of
  --   Just (Entity _ (User _ _ _ _ Nothing Nothing)) ->
  --     redirect SettingsR
  --   Just _ -> redirect HomeR -- already registered. go ahead
  --   Nothing -> redirect $ AuthR OA.twitterUrl -- weird. try again

getSignoutR :: Handler RepHtml
getSignoutR = do
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect HomeR
    Just aid -> do
      runDB $ update aid [UserOauthToken =. Nothing, UserOauthSecret =. Nothing]
      redirect $ AuthR LogoutR
