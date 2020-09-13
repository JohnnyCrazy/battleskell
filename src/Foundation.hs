{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

import Control.Monad.Logger (LogSource)
-- Used only when in "auth-dummy-login" setting is enabled.

import qualified Database.Esqueleto as E
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Model.Game
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth.HashDB (HashDBUser (..), authHashDBWithForm)
import Yesod.Auth.Message
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

data LobbyEvent
  = LobbyRemovedEvent GameId
  | LobbyAddedEvent (Entity Game, E.Value Text) -- snd is the game owner name
  deriving (Show, Generic)

data GameEvent
  = GameStateUpdatedEvent GameState
  | GameBattlefieldUpdatedEvent PlayerType BattlefieldStateView
  | GameLogUpdatedEvent (Entity GameLog)
  deriving (Show, Generic, Eq)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings :: AppSettings,
    -- | Settings for static file serving.
    appStatic :: Static,
    -- | Database connection pool.
    appConnPool :: ConnectionPool,
    appHttpManager :: Manager,
    appLogger :: Logger,
    -- lobby is a single TChan, since all clients share the same lobby screen
    lobbyChannel :: TChan LobbyEvent,
    -- game channels need to be seperated by id, thus we need a map of channels.
    -- it's not optiomal, since getting the channel for different games block the TVar via "atomically"
    gameChannels :: TVar (Map (Key Game) (TChan GameEvent))
  }

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
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a =
  forall (m :: * -> *).
  (MonadUnliftIO m) =>
  ReaderT SqlBackend m a

getUser :: Maybe (Entity User) -> Maybe User
getUser Nothing = Nothing
getUser (Just (Entity {entityVal})) = Just entityVal

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> defaultClientSessionBackend
        120 -- timeout in minutes
        "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware =
    defaultYesodMiddleware
      . ( \x -> do
            alreadyExpired
            x
        )

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    fullwidth <- isFullwidth <$> getCurrentRoute
    containerClassname <-
      if fullwidth
        then return ("w-full mx-auto py-4 px-4" :: Text)
        else return ("w-full mx-auto py-4 px-4 md:w-4/5 lg:w-8/12" :: Text)
    user <- getUser <$> maybeAuth
    pc <- widgetToPageContent $ do
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute :: App -> Maybe (Route App)
  authRoute _ = Just (AuthR LoginR)

  isAuthorized :: Route App -> Bool -> Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  -- isAuthorized CommentR _ = return Authorized
  isAuthorized HomeR _ = return Authorized
  isAuthorized RegisterR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized GamesR _ = fst <$> isAuthenticated
  isAuthorized (GameR _) True = fst <$> isAuthenticated
  isAuthorized (GameR gameId) _ = requireAuthenticatedAndGameParticipant gameId
  isAuthorized (GameStateR gameId) _ = requireAuthenticatedAndGameParticipant gameId

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    -- | The file extension
    Text ->
    -- | The MIME content type
    Text ->
    -- | The contents of the file
    LByteString ->
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
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
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

loginForm :: Route App -> Widget
loginForm action = do
  mErrors <- Just <$> maybeToList <$> getMessage
  $(widgetFile "login")

instance HashDBUser User where
  userPasswordHash = userPassword
  setPasswordHash h u = u {userPassword = Just h}

instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = HomeR

  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR

  authPlugins _ = [authHashDBWithForm loginForm (Just . UniqueUsername)]

  authenticate :: (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
  authenticate creds = liftHandler $
    runDB $ do
      x <- getBy $ UniqueUsername $ credsIdent creds
      case x of
        Just (Entity uid _) -> return $ Authenticated uid
        Nothing -> return $ UserError InvalidUsernamePass

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler (AuthResult, Maybe (Key User))
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> (Unauthorized "You must login to access this page", Nothing)
    Just uid -> (Authorized, Just uid)

instance YesodAuthPersist App where
  type AuthEntity App = User

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
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

instance (ToJSON a) => ToJSON (E.Value a) where
  toJSON (E.Value a) = toJSON a

requireAuthenticatedAndGameParticipant :: Key Game -> Handler AuthResult
requireAuthenticatedAndGameParticipant gameId = do
  (result, muid) <- isAuthenticated
  maybe (return $ result) (requireGameParticipant gameId) muid

-- only allow if he actually joined the game, no strangers in running games
requireGameParticipant :: Key Game -> Key User -> Handler AuthResult
requireGameParticipant gameId userId = do
  mgame <-
    runDB $
      selectFirst ([GameOwnerId ==. userId, GameId ==. gameId] ||. [GameGuestId ==. Just userId, GameId ==. gameId]) []
  case mgame of
    Nothing -> return $ Unauthorized "This game does not exist or you're not a member of this game!"
    Just (Entity _ _) -> return Authorized

-- helper for making the game view full width, while other screens have that nice left/right space for desktop apps
isFullwidth :: Maybe (Route App) -> Bool
isFullwidth (Just (GameR _)) = True
isFullwidth _ = False
