module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.OAuth2.Google (oauth2GoogleScopedWithCustomId, googleUid)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
-- Imports above this comment belong to the original scaffolding.

import Model.InvoiceProfile (maybeInvoiceProfile)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

--
-- ** Google API parameters
--

-- | Text referring to the actual value stored in the client session. The access
-- token is an Authorization key indicating user consent for server requests to
-- Google API's, to services that the user has granted access to. This token is
-- necessary to make any requests to Google APIs that involve any private
-- user data, or any actions normally beholden to the user alone.
--
-- For fetching the actual value from a session, see 'maybeAccessToken' from the
-- local GoogleAPIUtils module.
accessToken :: Text
accessToken = "access_token"

-- | For sending requests through google's developer API.
serverKey :: (IsString s) => s
serverKey = "AIzaSyABx4cyODtUI8wBx-CYVcEPV6ENNILpN1s"

{-
addServerKey :: (IsString s, Monoid s) => s -> s
addServerKey s = s ++ "?key=" ++ serverKey
-}

clientId :: Text
clientId = "574596987605-ep6t83nlm4he43ugm0cmrma50t3foff7.apps.googleusercontent.com"

clientSecret :: Text
clientSecret = "QvXX_Bp9MctdUkqQZfRnNPdX"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    -- Currently redirects the user directly to the Google Login page.
    authRoute _ = Just $ AuthR (PluginR "google" ["forward"])

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized

    -- ensures that a user can only view profiles that they themselves created
    isAuthorized (ViewInvoiceProfileR pid) _ = do
        uid <- requireAuthId
        muid' <- runDB $ (map invoiceProfileUserId) <$> maybeInvoiceProfile pid
        if  -- you are the creator of the entity
            | Just uid == muid' -> return Authorized
            | otherwise -> do
                addMessage "warning" "Not a valid link to your Invoice Profile"
                redirect InvoiceProfilesR

    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    -- Code to run after user logs in. Defaults to adding a message saying that
    -- they've successfully logged-in. We override this message to use our
    -- custom messaging system, which utilizes the Bootstrap Alert component.
    onLogin = setMessage "fix me"

    -- TODO: refine this function, heavily. Factor out code to other modules
    -- as well.
    authenticate creds = runDB $ do
        let ident = credsIdent creds
            extra = credsExtra creds
        x <- getBy $ UniqueUser $ ident

        now <- liftIO getCurrentTime

        -- These next two operations are used to set a token which is used in
        -- the authentication header to any request to a google API that
        -- requires a user's consent. This is part of the OAuth2 spec.
        let token = lookup "access_token" extra
        flip maybe (setSession accessToken)
            (error "Authentication worked, however, you do not have an access\
            \ token to enable features from Google. Please open a support\
            \ ticket, and we'll help to resolve this issue.")
            token

        case x of
            Just (Entity uid _) -> do
                return $ Authenticated uid
            Nothing -> do
                uid <- insert $ User
                    { userIdent = ident
                    , userName = lookup "name" extra
                    , userAvatarUrl = lookup "avatar_url" extra
                    , userCreatedAt = now }
                -- maybe add the user's email to the DB. There's no reason why
                -- this would fail, but it *technically* can, so...
                maybe (return ()) (insert_ . (\email -> Email
                    { emailUserId = Just uid
                    , emailEmail = email
                    }))
                    (lookup "email" extra)
                return $ Authenticated uid

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins _ = [
        oauth2GoogleScopedWithCustomId googleUid
            [ "https://www.googleapis.com/auth/calendar"
            , "https://www.googleapis.com/auth/userinfo.email"
            , "https://www.googleapis.com/auth/userinfo.profile"
            ] clientId clientSecret
        ]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
