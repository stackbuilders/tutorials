{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Monad.Catch (try)
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (ctrCombine)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random
import Data.ByteString (ByteString)
import Data.Serialize hiding (Get)
import Data.Text (Text)
import Lucid
import Network.URI hiding (scheme)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Servant.Server.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

----------------------------------------------------------------------------
-- General

-- | The handler monad, to work with cookies we need to have access to
-- 'AuthCookieSettings', 'RandomSource', and 'ServerKey'. It's also a handy
-- place to put some values that will be useful for route rendering.

type App = ReaderT AppContext (ExceptT ServantErr IO)

-- | The application's context.

data AppContext = AppContext
  { appContextAuthSettings :: AuthCookieSettings
  , appContextRandomSource :: RandomSource
  , appContextServerKey    :: ServerKey
  , appContextApproot      :: String
  , appContextPort         :: Int
  , appContextScheme       :: String
  }

-- | A 'Session' in our case will include the user name and his\/her email.
-- In a more realistic application this could as well include the user's id.
--
-- In order to store the 'Session' in a cookie, we need to make it an
-- instance of the 'Serialize' type class.

data Session = Session
  { sessionUsername :: Text
  , sessionEmail    :: Text }

instance Serialize Session where
  put (Session username email) = do
    let putText txt =
          let bytes = T.encodeUtf8 txt
          in (putWord32le . fromIntegral . B.length) bytes
             >> putByteString bytes
    putText username
    putText email
  get = do
    let getText =
          getWord32le >>= fmap T.decodeUtf8 . getBytes . fromIntegral
    username <- getText
    email    <- getText
    return (Session username email)

-- | The type family specifies which data type that ‘servant-auth-cookie’
-- library will work with. We have @'Maybe' 'Session'@ here to know when
-- session is just not present.

type instance AuthCookieData = Maybe Session

-- | The acutal session check. The ‘servant-auth-cookie’ package provides
-- the 'defaultAuthHandler', but that does not indicate missing 'Session' as
-- 'Nothing', so we use the custom one.

cookieAuthCheck
  :: AuthCookieSettings
  -> ServerKey
  -> AuthHandler Request (Maybe Session)
cookieAuthCheck authSettings serverKey = mkAuthHandler $ \request -> do
  result <- try (getSession authSettings serverKey request)
  case result :: Either AuthCookieException (Maybe Session) of
    Left _ -> return Nothing
    Right session -> return session

-- | The entry point of the application.

main :: IO ()
main = do
  let appPort = 8000
  randomSource <- mkRandomSource drgNew 2000
  serverKey    <- mkServerKey 16 (Just $ fromIntegral (86400 :: Integer))
  let authSettings = AuthCookieSettings
        { acsSessionField = "Session"
        , acsCookieFlags  = ["HttpOnly"]
        , acsMaxAge       = fromIntegral (6 * 3600 :: Integer)
        , acsExpirationFormat = "%0Y%m%d%H%M%S"
        , acsPath         = "/"
        , acsHashAlgorithm = Proxy :: Proxy SHA256
        , acsCipher       = Proxy :: Proxy AES256
        , acsEncryptAlgorithm = ctrCombine
        , acsDecryptAlgorithm = ctrCombine }
  run appPort . app $ AppContext
    { appContextAuthSettings = authSettings
    , appContextRandomSource = randomSource
    , appContextServerKey    = serverKey
    , appContextApproot      = "localhost"
    , appContextPort         = appPort
    , appContextScheme       = "http:" }

app :: AppContext -> Application
app appContext@AppContext {..} = serveWithContext
  (Proxy :: Proxy Routes)
  ((cookieAuthCheck appContextAuthSettings appContextServerKey
     :: AuthHandler Request (Maybe Session))
   :. EmptyContext)
  (server appContext)

server :: AppContext -> Server Routes
server context = enter (Nat $ flip runReaderT context) handlers

----------------------------------------------------------------------------
-- Routes

-- | It's generally a good idea to have a type synonym for your
-- authentication type so it's easier to modify it later.

type AppAuth = AuthProtect "cookie-auth"

-- This orphan instance is necessary in order to teach Servant how to render
-- routes that use our authentication method.

instance HasLink sub => HasLink (AppAuth :> sub) where
  type MkLink (AppAuth :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)

type Routes = GetHomeR
  :<|> GetSignInR
  :<|> GetMyPageR
  :<|> GetSignOutR

type GetHomeR =
  AppAuth :>
  Get '[HTML] (AppView HomeView)

type GetSignInR =
  "sign-in" :>
  Get '[HTML] (Headers '[Header "set-cookie" ByteString] (AppView SignInView))

type GetMyPageR =
  AppAuth :>
  "my-page" :>
  Get '[HTML] (AppView MyPageView)

type GetSignOutR =
  AppAuth :>
  "sign-out" :>
  Get '[HTML] (Headers '[Header "set-cookie" ByteString] (AppView SignOutView))

----------------------------------------------------------------------------
-- Views

-- | Type of the wrapper view (sort of “default template”). This one needs
-- to have rendered links in order to interpolate them into the HTML data it
-- generates.

data AppView view = AppView
  { appViewContent     :: view
  , appViewSession     :: Maybe Session
  , appViewHomeLink    :: Text
  , appViewSignInLink  :: Text
  , appViewMyPageLink  :: Text
  , appViewSignOutLink :: Text
  }

instance ToHtml view => ToHtml (AppView view) where
  toHtml AppView {..} = doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [httpEquiv_ "x-ua-compatible", content_ "IE=edge"]
      meta_ [name_ "viewport", content_ "width=device-width,initial-scale=1"]
    body_ $ do
      h1_ "Servant Auth Cookie Demo"
      hr_ []
      ul_ $ do
        li_ (a_ [href_ appViewHomeLink] "Home")
        case appViewSession of
          Nothing ->
            li_ (a_ [href_ appViewSignInLink] "Sign In")
          Just Session {..} -> do
            li_ (a_ [href_ appViewMyPageLink] "My Page")
            li_ (a_ [href_ appViewSignOutLink] "Sign Out")
      hr_ []
      toHtml appViewContent
  toHtmlRaw = toHtml

-- | Home page does not have anything special on it.

data HomeView = HomeView

instance ToHtml HomeView where
  toHtml HomeView = do
    h1_ "Home Page"
    "Welcome Home!"
  toHtmlRaw = toHtml

-- | Sign in view can show a message.

data SignInView = SignInView Text

instance ToHtml SignInView where
  toHtml (SignInView msg) = do
    h1_ "Sign In"
    "The app says:"
    strong_ (toHtml msg)
  toHtmlRaw = toHtml

data MyPageView = MyPageView Text Text

instance ToHtml MyPageView where
  toHtml (MyPageView username email) = do
    h1_ "My Page"
    "This is a page of user with the following data:"
    ul_ $ do
      li_ ("Username:" >> strong_ (toHtml username))
      li_ ("Email:"    >> strong_ (toHtml email))
  toHtmlRaw = toHtml

data SignOutView = SignOutView Text

instance ToHtml SignOutView where
  toHtml (SignOutView msg) = do
    h1_ "Sign Out"
    "The app says:"
    strong_ (toHtml msg)
  toHtmlRaw = toHtml

----------------------------------------------------------------------------
-- Helpers

-- | Get textual representation for specific endpoint on the site.

routeToText :: (IsElem a Routes, HasLink a, MkLink a ~ URI)
  => Proxy a           -- ^ The 'Proxy' clarifying type of route to render
  -> App Text      -- ^ The rendered route as 'Text'
routeToText = renderURI . routeToURI

-- | Get link representation for specific endpoint on the site.

routeToURI :: (IsElem a Routes, HasLink a) => Proxy a -> MkLink a
routeToURI = safeLink (Proxy :: Proxy Routes)

-- | Render an 'URI' as 'Text'.

renderURI :: URI -> App Text
renderURI uri = do
  approot <- asks appContextApproot
  port    <- asks appContextPort
  scheme  <- asks appContextScheme
  let uri' = uri
        { uriScheme = scheme
        , uriAuthority = Just URIAuth
          { uriUserInfo = ""
          , uriRegName = approot
          , uriPort = ':' : show port }
        , uriPath = '/' : uriPath uri }
  (return . T.pack) (uriToString (const "") uri' "")

-- | Create an 'AppView' this is the recommended method to created it, since
-- it initializes fields like 'appViewHomeLink' for you.

mkAppView
  :: Maybe Session     -- ^ Active 'Session' if any
  -> view              -- ^ Actual view to insert into 'AppView'
  -> App (AppView view) -- ^ The resulting view
mkAppView appViewSession appViewContent = do
  appViewHomeLink    <- routeToText (Proxy :: Proxy GetHomeR)
  appViewSignInLink  <- routeToText (Proxy :: Proxy GetSignInR)
  appViewMyPageLink  <- routeToText (Proxy :: Proxy GetMyPageR)
  appViewSignOutLink <- routeToText (Proxy :: Proxy GetSignOutR)
  return AppView {..}

-- | Perform actions with 'Session' or return 403 HTTP status code.

withSession
  :: Maybe Session     -- ^ 'Session', if any
  -> (Session -> App a) -- ^ Callback making use of 'Session'
  -> App a
withSession ms action = maybe (throwError err403) action ms

----------------------------------------------------------------------------
-- Handlers

-- | The collection of all handlers.

handlers :: ServerT Routes App
handlers = getHomeR
  :<|> getSignInR
  :<|> getMyPageR
  :<|> getSignOutR

-- | The home page does not do anything fancy, although just like all the
-- other pages it features the menu that changes depending on whether the
-- user is logged in or not.

getHomeR :: Maybe Session -> App (AppView HomeView)
getHomeR ms = mkAppView ms HomeView

-- | The “Sign In” handler set the cookie and displayes a message.

getSignInR :: App (Headers '[Header "set-cookie" ByteString] (AppView SignInView))
getSignInR = do
  let session = Session "mark" "mark@example.org"
  AppContext {..} <- ask
  mkAppView (Just session) (SignInView "You have signed in.")
    >>= addSession appContextAuthSettings appContextRandomSource appContextServerKey session

-- | Due to the use of the 'withSession' helper, this page is only available
-- to logged in users.

getMyPageR :: Maybe Session -> App (AppView MyPageView)
getMyPageR ms = withSession ms $ \Session {..} ->
  mkAppView ms (MyPageView sessionUsername sessionEmail)

-- | The “Sign Out” page sets cookies to empty byte string destroyng the data.

getSignOutR
  :: Maybe Session
  -> App (Headers '[Header "set-cookie" ByteString] (AppView SignOutView))
getSignOutR ms = withSession ms $ \_ -> do
  AppContext {..} <- ask
  mkAppView Nothing (SignOutView "You have signed out.")
    >>= addSession appContextAuthSettings appContextRandomSource appContextServerKey ()
