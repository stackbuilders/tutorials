---
title: Servant authentication and sessions via cookies
published: 2016-09-21
ghc: 7.10.3
lts: 6.10
tags: haskell
libraries: servant-auth-cookie-0.3.0.2 servant-0.8
language: haskell
author: Mark Karpov
author-name: Mark Karpov
description: Authentication is a sort of weak place in the Servant web framework. In this tutorial we build RESTful authentication in Servant storing all the session in an encrypted cookie client-side.
---

Authentication in Servant is perhaps not as easy and powerful as it should
be. However, with Servant 0.5 and later it's possible to use the feature
called “generalized authentication” to add authentication that is closer to
real-world expectations than anything before. The feature is called
“experimental” in the Servant docs with the hope to get feedback from users
to see how good the idea is. In this tutorial we will provide that feedback
going through building a small application that uses Servant for serving
user-facing content, not an API. We will be using the
[`servant-auth-cookie`](https://hackage.haskell.org/package/servant-auth-cookie)
library which we found and contributed to while adding authentication for
one of our projects at Stack Builders.

To follow the tutorial, familiarity with the
[Servant web-framework](https://haskell-servant.github.io/) is expected.

## Setting the goal

Currently
[the Servant docs](http://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html)
describe two ways to perform authentication with the framework: using
[basic authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
and with the above-mentioned generalized authentication. Both methods are
similar in how you use them in your code, yet generalized authentication
opens the possibility to read the cookie you might have previously set,
because it has access to request data.

In this tutorial we will set and then use for authentication an encrypted
cookie with (possibly) arbitrary session data in it. All the session data
will be stored client-side. It seems to make sense to check cookie and
return a `Maybe Session` value to give the handlers more freedom in how to
use session data or the fact of its absence (for example, we will change the
application menu depending on whether the user is logged in or not — a
reasonable thing to expect from a web application).

## Imports and language extensions

Let's start by importing some modules and enabling a few language
extensions. The complete source code of this application is available in the
[Stack Builders tutorials repository on GitHub](https://github.com/stackbuilders/tutorials),
so you can just clone the repository and start playing with it.

```haskell
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
```

## The handler monad

To work with cookies, we will need to be able to access at least three
values from handlers: `AuthCookieSettings` (more on that below),
`RandomSource`, and `ServerKey`. For route rendering we will need three more
things: approot (as a `String`), port (`Int`), and scheme (`String`).
Knowing these requirements it's an obvious choice to use a monad stack with
`ReaderT` parametrized over the `AppContext` record containing all necessary
data that should be available in handlers:

```haskell
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
```

This should be pretty self-explanatory, so let's move on to the session data.

## Session data

First of all it's necessary to decide what data to include in the session.
It will be stored in encrypted form on client's machine in a cookie which
means that every time a client makes a request to your domain, all the data
will be sent to you — this is why we should not store anything big there. In
most cases an identifier of the logged in user and some basic information
that forms context of current session is stored. In our example, we will
store user name and email:

```haskell
-- | A 'Session' in our case will include the user name and his\/her email.
-- In a more realistic application this could as well include the user's id.
--
-- In order to store the 'Session' in a cookie, we need to make it an
-- instance of the 'Serialize' type class.

data Session = Session
  { sessionUsername :: Text
  , sessionEmail    :: Text }
```

To encrypt and send the information we need to know how to convert it into a
`ByteString` — binary representation of the data. The `servant-auth-cookie`
package uses the [`cereal`](https://hackage.haskell.org/package/cereal)
library for serialization, so we need to make `Session` an instance of
`Serialize` type class:

```haskell
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
```

## Auth check and some boilerplate

The next thing we are going to need is to specify the type of data we will
get as a result of `cookieAuthCheck`. The `servant-auth-cookie` package
defines the `AuthCookieData` open type family for that. Let's define a type
instance:

```haskell
type instance AuthCookieData = Maybe Session
```

Now we are ready to write the check whose type is `AuthHandler Request
(Maybe Session)`:

```haskell
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
```

Now we can check whether the cookie is present and can be de-serialized or
not. Then goes some boilperplate:

```haskell

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
```

Note that we create `RandomSource` which is a source of random numbers that
will reset itself after producing 2000 random bytes. The source can be used
in concurrent code because the actual modification of its internal state is
atomic. `ServerKey` is another thing we are going to need for cookie
encryption, it has the ability to “expire” after specified period of time
(one day in our case, but if we passed `Nothing` it would never expire).
It's suitable for concurrent usage as well.

Other things to note is the use of `serveWithContext` that is
well-documented in the Servant documentation
[here](https://www.stackage.org/haddock/nightly-2016-08-06/servant-server-0.7.1/Servant-Server.html#v:serveWithContext)
and the `enter` function that given natural transformation from `App` monad
to Servant's default `Handler` monad allows us to use our custom monad in
handlers.

## Defining the routes

First, let's define a type synonym for the type that will represent auth
check in our routes:

```haskell
-- | It's generally a good idea to have a type synonym for your
-- authentication type so it's easier to modify it later.

type AppAuth = AuthProtect "cookie-auth"

```

We are going to render the routes. For that to work nicely with `AppAuth`,
we need to define the following orphan instance:

```haskell
-- This orphan instance is necessary in order to teach Servant how to render
-- routes that use our authentication method.

instance HasLink sub => HasLink (AppAuth :> sub) where
  type MkLink (AppAuth :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)

```

The rest is the definitions of all the endpoints our application is going to
have.

```haskell
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
```

Each of them is protected with `AppAuth` even though only the last two
really require protection. The `GetHomeR` route is protected to be able to
change the main menu for logged in users. The `GetSignInR` does not require
authentication in our application because when the page in rendered the user
is always logged in and we know how to render the page.

## Defining views and their HTML representation

Every route will return a value of distinct type that should be convertable
to HTML, since we use [`lucid`](https://hackage.haskell.org/package/lucid)
for building HTML data, we will need to make the views instances of the
`ToHtml` type class:

```haskell
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
```

## Useful helpers for handler writing

Before we go on to write the actual handlers, we are going to need a number
of helpers that seems to be very handy in this application and even more
handy in real ones.

The first collection of functions deal with route rendering.

```haskell
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
```

Here we are interested mainly in `routeToText`, because our routes do not
have captures or query parametrs and thus values produced by `safeLink`
(provided by Servant itself) return us `URI` values directly. The `safeLink`
function is pure, but to render a link to `Text` we need things like
approot, port number, and scheme which come from the `App` monad, hence the
type.

The `mkAppView` function helps with populating links-related fields of
`AppView` record using the `routeToText` function we just defined:

```haskell
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

```

Lastly, since we get `Nothing` when the cookie is not set or cannot be
deserialized, even non-authorized calls get to handlers. To ensure that only
logged-in users can receive responses from some handlers the following
simple helper can be used:

```haskell
-- | Perform actions with 'Session' or return 403 HTTP status code.

withSession
  :: Maybe Session     -- ^ 'Session', if any
  -> (Session -> App a) -- ^ Callback making use of 'Session'
  -> App a
withSession ms action = maybe (throwError err403) action ms
```

Now we are set for success in writing our handlers!

## Defining handlers

It's time to use all the goodies we have defined so far to put together our
handlers:

```haskell
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
```

As you can see the only interesting moment here is the use of `addSession`
function that comes from the `servant-auth-cookie` package. Using the values
from the `App` monad such as cookie settings and server key we encrypt the
binary data of serialized `Session` and add it to the response we are going
to send to the client.

For the sake of simplicity we don't show a “Sign In” form here, although it
could be added trivially. Every time you request the “Sign In” page the app
creates a cookie for the same user named `"mark"`. Nothing stops you from
performing a database lookup here and setting the real data.

## Running the application

To run the app we need to compile it, and execute the `servant-auth`
executable (if you cloned our repository, you may have called the executable
differently if you are recreating the application on your own):

```bash
$ stack build
$ stack exec servant-auth
```

Then go to `localhost:8000` in your browser, you should see something like
this:

![Home page of our app](/tutorials/haskell/servant-auth/home-page.png)

Right now you are not logged in. To log in follow the “Sign In” link — you
will notice that menu items have changed accordingly. Now you have access to
“My Page” and “Sign Out” endpoints that were not present on the menu
previously and in fact were completely inaccessible for you. Visiting the
“Sign Out” endpoint returns you to the initial state.

## Conclusion

We have achieved our goal and created a usable user interface staying close
to Servant's philosophy. The “generalized authentication” of Servant
certainly can be used to craft any sort of authentication, although the
feature still has a strong “experimental” flavor in it and required some
tweaking in our case.
