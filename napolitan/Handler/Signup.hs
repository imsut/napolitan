{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Signup where

import Import
import qualified Yesod.Auth.OAuth as OA
import Yesod.Logger
import Yesod.Auth

getSignupR :: Handler RepHtml
getSignupR = do
  mauth <- maybeAuth
  case mauth of
    Just (Entity _ (User _ _ _ _ Nothing)) ->
      redirect SettingsR
      --liftIO $ defaultDevelopmentLogger >>= flip logString (show u)
    Just _ -> redirect HomeR -- already registered. go ahead
    Nothing -> redirect $ AuthR OA.twitterUrl -- weird. try again

getSignoutR :: Handler RepHtml
getSignoutR = do
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect HomeR
    Just aid -> do
      runDB $ update aid [UserOauthToken =. Nothing, UserOauthSecret =. Nothing]
      redirect $ AuthR LogoutR
