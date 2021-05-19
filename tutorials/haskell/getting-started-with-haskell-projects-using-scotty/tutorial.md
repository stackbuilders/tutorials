---
title: Getting started with Haskell projects using Scotty
published: 2021-05-25
tags: haskell
language: haskell
author-name: Juan Pedro Villa Isaza
twitter-profile: jpvillaisaza
github-profile: jpvillaisaza
description:
  In this tutorial, we show how to create a Haskell project from
  scratch using the Scotty web framework. As an example, we use a URL
  shortener to introduce and motivate web development in Haskell.
---

As part of our internal [Haskell][haskell] training, we use a URL
shortener as a way to get started with Haskell projects and web
applications in particular. We recommend using the [Scotty][scotty]
web framework, but other frameworks can be used as well. This tutorial
is a detailed walk-through of how to build the URL shortener from
scratch.

[haskell]: https://www.haskell.org
[scotty]: https://hackage.haskell.org/package/scotty

As a note, we use this exercise after a few weeks of training, so some
familiarity with Haskell, but not Haskell projects, is expected. For
this, we're currently using part I of [Programming in Haskell][pih] by
Graham Hutton or the corresponding chapters from [Learn You a Haskell
for Great Good!][lyah] by Miran Lipovača, but there's a lot of
[Haskell documentation][haskell-doc] to choose from!

[haskell-doc]: https://www.haskell.org/documentation/
[lyah]: http://learnyouahaskell.com
[pih]: http://www.cs.nott.ac.uk/~pszgmh/pih.html

Before we begin, here's a list of the versions used for preparing
this tutorial:

* [LTS Haskell 17.10][lts-17.10] ([GHC][ghc] 8.10.4)
* [Stack][stack] 2.7.1 and [Cabal][cabal] 3.4.0.0
* [Scotty 0.12][scotty-0.12]

[cabal]: https://www.haskell.org/cabal/
[ghc]: https://www.haskell.org/ghc/
[lts-17.10]: https://www.stackage.org/lts-17.10
[scotty-0.12]: https://hackage.haskell.org/package/scotty-0.12
[stack]: https://haskellstack.org/

## Setting up a Haskell project

To recap, we're going to create a URL shortener using Scotty. The idea
is to have a page where a user can submit a URL and get a shortened
version, which they can then access and be redirected to the original
URL.

First, we need a Haskell project, which we're going to set up using
both Stack and Cabal (but you only need to use one of them). For
Stack, create a directory for the project (`shortener`) and add a
`stack.yaml` file, as follows:

```sh
mkdir shortener
cd shortener
echo "resolver: lts-17.10" > stack.yaml
```

As of writing, [LTS 17.10][lts-17.10] is the most recent resolver,
which uses GHC 8.10.4. To choose a different resolver, go to
[Stackage][stackage] and check the latest releases (or run `stack ls
snapshots --remote` to get a similar list). For Cabal, skip the
`stack.yaml` file part.

[stackage]: https://www.stackage.org

Next, we need a Cabal file (`shortener.cabal`), which we'll create
with very little data (a lot more can be added, but this enough for
our application and close to a minimal Cabal file):

```cabal
cabal-version: 2.2

name: shortener
version: 0.1.0.0

common common
  build-depends:
      base >= 4.14
  default-language: Haskell2010
  ghc-options: -Wall

library
  import: common
  hs-source-dirs: src
  exposed-modules: Shortener
  build-depends:
      scotty >= 0.12

executable shortener
  import: common
  hs-source-dirs: app
  main-is: Main.hs
  build-depends:
      shortener
```

We'll add more things to this file later on. Let's review some of it:

* The `cabal-version` line specifies the version of the Cabal file
  format we want to use. In this case, we're using version 2.2 so that
  we can use common stanzas, but note that this is not the most recent
  version. This line is required to be the first line of the Cabal
  file.
* The `name` and `version` lines include the name and version of the
  library associated to the Cabal file. A Cabal file defines one
  library and zero or more executables (which can be executables,
  tests, or benchmarks).
* The `common common` block defines a common stanza called `common`,
  which we can reuse to list common dependencies (in this case, the
  `base` library), the default language, and compiler options (in this
  case, `-Wall` to enable all warnings):
  - The default language is normally `Haskell2010`, which is the
    current definition of the Haskell language. If using only Cabal,
    this line can be removed after updating the `cabal-version` to
    3.4, which uses `Haskell2010` by default.
  - The [`base`][base] library is the Haskell standard library, which
    defines the `Prelude`, and also works to specify the version of
    the compiler that we want to support. In this case, `base >= 4.14`
    means that we support GHC 8.10.1 or higher. For more information
    about the `base` library and the `Prelude`, see [What I Wish I
    Knew When Learning Haskell's `base` section][diehl-base] or [the
    Type Classes Prelude][typeclasses-prelude] reference.
* The `library` block defines the library component. We import the
  common stanza we already talked about (which has to be the first
  item of the block), choose `src` as the directory for the source
  files, and only specify one module called `Shortener` (together, the
  last two mean that there must be a file `src/Shortener.hs` with a
  module called `Shortener`). Finally, we add `scotty` as a dependency
  using the version in LTS 17.10 as a lower constraint.
* The `executable` block defines an executable with name `shortener`
  (the same name as the library), which is the one we can run after
  compiling the application. The rest of the block is similar to the
  `library` block, except that the only dependency is the library and
  that there must be a file `app/Main.hs` with a `Main` module.

[base]: https://hackage.haskell.org/package/base
[typeclasses-prelude]: https://typeclasses.com/prelude
[diehl-base]: http://dev.stephendiehl.com/hask/#base

In order for everything to work with the current setup, let's create
the needed files without the actual solution. Here's
`src/Shortener.hs`:

```haskell
module Shortener where

shortener :: IO ()
shortener =
  putStrLn "Shortener"
```

And here's `app/Main.hs`:

```haskell
module Main (main) where

import Shortener (shortener)

main :: IO ()
main =
  shortener
```

In `Shortener`, we define `shortener` for what we'll implement and
print the word "Shortener" for now. In `Main`, we call `shortener`,
which means that our `Main` module is a wrapper for `shortener` and we
can forget about it for the rest of the tutorial.

With all of this in place, you can run `stack build` or `cabal build`
to install dependencies and build the application. To run the
application, run `stack run` or `cabal run`.

## Hello, Scotty!

The [Scotty repository][scotty-gh] includes a small example to get
started, and additional documentation and examples (including a [URL
shortener][scotty-shortener] that we'll use as a base for our
solution). Based on this, let's update the `Shortener` module to
display the word "Shortener" instead of printing it:

[scotty-gh]: https://github.com/scotty-web/scotty
[scotty-shortener]: https://github.com/scotty-web/scotty/blob/master/examples/urlshortener.hs

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Web.Scotty

shortener :: IO ()
shortener =
  scotty 3000 $
    get "/" $
      html "<h1>Shortener</h1>"
```

Let's go over the contents of the file:

* The line at the beginning is called a language pragma and extends
  the languauge with nice features. In this case, `OverloadedStrings`
  allows us to write `"hello"` and it gets automatically converted to
  the string type we need (`String`, `ByteString`, or `Text`).
  - For more information about language pragmas and overloaded
    strings, see [Kowainik's Extensions post][kowainik-ex] or the
    [Type Classes Introduction to GHC extensions][typeclasses-ex]
* `scotty` is the entry function that Scotty defines for running an
  application. The first parameter is the port that we want it to run
  in, and the rest is the application, which looks like a list of
  routes and handlers.
* For now, we only have one route (the root) and a handler, which is
  a GET and returns an HTML string with a title.

[kowainik-ex]: https://kowainik.github.io/posts/extensions
[typeclasses-ex]: https://typeclasses.com/extensions-intro

If everything works as expected, you can build and run the application
using `stack run` or `cabal run`, which should display a message
similar to the following:

```sh
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

Open `http://localhost:3000/` and see the title.

Before moving on, let's use an HTML-based templating engine to
generate HTML in Haskell. There are several libraries, but we're going
to use [`blaze-html`][blaze-html]. To do so, we need to add it as a
dependency to our Cabal file, as follows (note that this just the
dependencies part of the file for the library component):

[blaze-html]: https://hackage.haskell.org/package/blaze-html

```cabal
build-depends:
    blaze-html >= 0.9
  , scotty >= 0.12
```

As Blaze is not the main part of the exercise, you can check out the
[BlazeHtml tutorial][blaze-html-tutorial]. Here's the updated
application:

[blaze-html-tutorial]: https://jaspervdj.be/blaze/tutorial.html

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Web.Scotty

shortener :: IO ()
shortener =
  scotty 3000 $
    get "/" $
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Shortener"
```

Instead of raw HTML, we now have Haskell functions that look like an
HTML document. The main advantage of this is that we can get type
errors if we do something wrong. If you rebuild and run, and open your
browser, the result should be the same as before.

## Shortening URLs

For the URL shortener, we need 3 things:

* A landing page with a form to input a URL (we have a page, but not
  a form)
* A POST request to handle the submitted form and create a shortened
  version of a URL
* A GET request to handle a shortened URL and redirect to the original
  URL

Let's add the form, which is mostly HTML. Take a look at the Haskell
code:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

shortener :: IO ()
shortener =
  scotty 3000 $
    get "/" $
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"
```

This will get transformed to this output HTML form:

```html
<form method="post" action="/">
  <input type="text" name="url">
  <input type="submit">
</form>
```

If you rebuild and run the application, you should see the form. If
you submit something, it should fail because there's no handler for
the POST request. Let's add it!

To do so, we have to store the shortened URL somewhere. It should be a
database, but we'll use an `IORef`, which is a mutable reference to a
value (see [What I Wish I knew When Learning Haskell's section on
`IORef`][diehl-ioref] for more information). We're going to store the
URLs as `Text` in a `Map`, so we need two additional dependencies,
`containers` and `text`. Here are the updated library dependencies:

[diehl-ioref]: http://dev.stephendiehl.com/hask/#ioref

```cabal
build-depends:
    blaze-html >= 0.9
  , containers
  , scotty >= 0.12
  , text
```

The `containers` and `text` libraries are boot libraries, which means
they're already installed with the compiler and the `base` library. We
don't specify version contraints for these, but we can find the
version used in the [GHC Boot Library Version History][ghc-boot].

[ghc-boot]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history

Here's the new version of the program:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (modifyIORef, newIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

shortener :: IO ()
shortener = do
  urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"
    post "/" $ do
      url <- param "url"
      liftIO $ modifyIORef urlsR $
        \(i, urls) ->
          (i + 1, M.insert i url urls)
      redirect "/"
```

Before creating the Scotty app, we create an `IORef` to hold two
things: the current index of the URLs created (we're using numbers, so
shortened URLs will look like `http://localhost:3000/1`), and the map
of numbers to original URLs. A `Map` is like a list of key-value
tuples, which in this case uses numbers as keys and URLs as values.
For the POST request, we get the URL from the input field with name
`url` and modify the map of URLs using `modifyIORef` by increasing the
current index by 1 and inserting the new URL to the map.

If you rebuild and run the application, you can now submit the form
and everything seems to work, but how do we know that the shortened
URL was created? Let's add a list of URLs to the main page to see how
to read data from the `IORef`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

shortener :: IO ()
shortener = do
  urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsR
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"
            H.table $
              for_ (M.toList urls) $ \(i, url) ->
                H.tr $ do
                  H.td (H.toHtml i)
                  H.td (H.text url)
    post "/" $ do
      url <- param "url"
      liftIO $ modifyIORef urlsR $
        \(i, urls) ->
          (i + 1, M.insert i url urls)
      redirect "/"
```

We get the map of URLs using `readIORef` and only use the second part
(the current index is only needed when creating a shortened URL). We
display the list of URLs using an HTML table with a row for each URL.
Now, after submitting the form, the list should update!

But what happens if you shorten a URL and then try to open the
shortened URL (`http://localhost:3000/1`)?

Can you implement the missing GET handler to redirect the user to the
original URL? After doing so, here's one possible solution:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (status404)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

shortener :: IO ()
shortener = do
  urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsR
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"
            H.table $
              for_ (M.toList urls) $ \(i, url) ->
                H.tr $ do
                  H.td (H.toHtml i)
                  H.td (H.text url)
    post "/" $ do
      url <- param "url"
      liftIO $ modifyIORef urlsR $
        \(i, urls) ->
          (i + 1, M.insert i url urls)
      redirect "/"
    get "/:n" $ do
      n <- param "n"
      (_, urls) <- liftIO $ readIORef urlsR
      case M.lookup n urls of
        Just url ->
          redirect (LT.fromStrict url)
        Nothing ->
          raiseStatus status404 "not found"
```

This solution works, but there are some issues with it that can be
used as exercises:

* For a production-ready solution, you should switch to a database
  instead of using an `IORef`. Can you update the application to use
  a database instead of an `IORef`? There are some examples in [What I
  Wish I Knew When Learning Haskell's databases section][diehl-db].
* When submitting the form, there's no validation of the URL, so
  anything is accepted. Can you validate the URL and reject invalid
  ones?
* When a URL is not found, using `status404` requires updating the
  dependencies to include `http-types`, as suggested by the compiler
  if you try to build the proposed solution. Can you update the Cabal
  file to make it work?

[diehl-db]: http://dev.stephendiehl.com/hask/#databases

We hope you have fun with Scotty and web development in Haskell, and
remember to also try other Haskell web frameworks or use your own idea
for a web application! For even more information about Haskell
projects and a different walk-through of a URL shortener, see chapters
13 (Building projects) and 19.6 (An end-to-end example: URL shortener)
of the [Haskell Programming from First Principles][haskellbook] book.

[haskellbook]: https://haskellbook.com
