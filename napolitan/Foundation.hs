module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText, logString, defaultDevelopmentLogger)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import qualified Yesod.Auth.OAuth as OA
import Data.Maybe
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Control.Arrow ((***))

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        mauth <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_bootstrap_responsive_css
            addStylesheet $ StaticR css_napolitan_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = SignupR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
      liftIO $ defaultDevelopmentLogger >>= (flip logString $ show $ credsExtra creds)
      let userId = credsIdent creds
          extra = credsExtra creds
          screenName = fromJust $ lookup "screen_name" extra
          moaToken = lookup "oauth_token" extra
          moaSecret = lookup "oauth_token_secret" extra
      x <- getBy $ UniqueUser userId
      case x of
        Just (Entity uid (User _ _ mtoken msecret _)) ->
          if mtoken == moaToken && msecret == moaSecret
          then return $ Just uid
          else do
            update uid [UserOauthToken =. moaToken, UserOauthSecret =. moaSecret]
            return $ Just uid
        Nothing -> do
          fmap Just $ insert $ User userId screenName moaToken moaSecret Nothing

    authPlugins m = [authTwitter
                     ((encodeUtf8 . extraOauthKey . appExtra . settings) m)
                     ((encodeUtf8 . extraOauthSecret . appExtra . settings) m)]

    authHttpManager = httpManager

    -- override default behavior, setting login message
    onLogin = return ()

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Auth plugin for Twitter OAuth
authTwitter :: YesodAuth m => ByteString -> ByteString -> AuthPlugin m
authTwitter key secret = OA.authOAuth
                (OA.newOAuth { OA.oauthServerName      = "twitter"
                          , OA.oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
                          , OA.oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
                          , OA.oauthAuthorizeUri    =  "https://api.twitter.com/oauth/authorize"
                          , OA.oauthSignatureMethod = OA.HMACSHA1
                          , OA.oauthConsumerKey     = key
                          , OA.oauthConsumerSecret  = secret
                          , OA.oauthVersion         = OA.OAuth10
                          })
                extractCreds
  where
    extractCreds (OA.Credential dic) = do
        let crId = bsToText $ fromJust $ lookup "user_id" dic
        return $ Creds "twitter" crId $ map (bsToText *** bsToText ) dic

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode

