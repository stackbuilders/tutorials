---
title: File uploads in (a) Snap
published: 2016-08-31
ghc: 7.10.3
lts: 6.10
libraries: snap-core-1.0.0.0 snap-server-1.0.0.0
language: Haskell
author-name: Juan Pedro Villa Isaza
---

We recently had to add file uploads to a
[Haskell](https://www.haskell.org/) web application built with the
[Snap](http://snapframework.com/) framework. File uploads in Snap (that
is,
[`Snap.Util.FileUploads`](https://hackage.haskell.org/package/snap-core-1.0.0.0/docs/Snap-Util-FileUploads.html))
have good documentation, but we thought it would be useful to write a
tutorial about how to implement a basic file uploader. The idea is to
write an application with an HTML form to upload multiple Haskell files
(`.hs` or `.lhs`) that simply displays the contents of the uploaded
files.

This blog post was generated from a literate Haskell file that you can
find in the
[turbulent-sniffle](https://github.com/stackbuilders/turbulent-sniffle)
repository. All the code was tested with [LTS Haskell
6.10](https://www.stackage.org/lts-6.10), that is, GHC 7.10.3,
[snap-core
1.0.0.0](https://hackage.haskell.org/package/snap-core-1.0.0.0), and
[snap-server
1.0.0.0](https://hackage.haskell.org/package/snap-server-1.0.0.0).

The first thing we'll do is create an `index.html` file and add an HTML
form to upload multiple files:

```html
<form action="/" method="post" enctype="multipart/form-data">
  <input type="file" name="files" multiple>
  <button type="submit">Submit</button>
</form>
```

This is just an HTML form with a file input that allows multiple files.
We could make the input required, but we'll use Haskell to handle
everything.

In order to see exactly what we need for adding file uploads to an
existing Haskell application, let's create a Cabal file. In this case,
we'll call it `turbulent-sniffle.cabal`:

    name: turbulent-sniffle
    version: 0.1.0
    build-type: Simple
    cabal-version: >= 1.22

We can now add the `index.html` file as a data file:

    data-files: index.html

And create an executable component:

    executable turbulent-sniffle
      main-is: Main.lhs
      other-modules: Paths_turbulent_sniffle
      ghc-options: -Wall
      build-depends:
          base >= 4.8 && < 4.9
        , bytestring
        , directory
        , snap-core >= 1.0 && < 1.1
        , snap-server >= 1.0 && < 1.1
        , text >= 1.2 && < 1.3
        , transformers
      default-language: Haskell2010

If you cloned the
[turbulent-sniffle](https://github.com/stackbuilders/turbulent-sniffle)
repository, you can now build and run the application:

    $ stack build --exec turbulent-sniffle

Using the following `stack.yaml` file:

    extra-deps:
    - io-streams-haproxy-1.0.0.0
    - snap-core-1.0.0.0
    - snap-server-1.0.0.0
    resolver: lts-6.10

Go to <http://localhost:8000/> and try to upload a Haskell file such as
`Main.lhs`.

Next, let's take a look at the Haskell code in `Main.lhs`, starting
with the module declaration and the list of imports:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main
 ( main
 )
 where

-- turbulent-sniffle
import qualified Paths_turbulent_sniffle as Paths

-- base
import Control.Applicative ((<|>))
import qualified Data.Either as Either
import Data.Int (Int64)
import qualified Data.Maybe as Maybe

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteStringChar8

-- directory
import qualified System.Directory as Directory

-- snap-core
import Snap.Core
  ( Method(GET, POST)
  , Snap
  )
import qualified Snap.Core as SnapCore
import qualified Snap.Util.FileServe as SnapFileServe
import Snap.Util.FileUploads
  ( PartInfo
  , PartUploadPolicy
  , PolicyViolationException
  , UploadPolicy
  )
import qualified Snap.Util.FileUploads as SnapFileUploads

-- snap-server
import qualified Snap.Http.Server as SnapServer

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- transformers
import qualified Control.Monad.IO.Class as MonadIO
```

The
[snap-server](https://hackage.haskell.org/package/snap-server-1.0.0.0)
package allows us to quickly serve an application, as follows:

``` {.sourceCode .haskell}
main :: IO ()
main =
  SnapServer.quickHttpServe
    (SnapCore.ifTop (SnapCore.writeBS "Behold a turbulent sniffle!"))
```

This application would simply print a string, which is not what we want.
Instead, let's add a handler for the top (`/`) route:

```haskell
main :: IO ()
main =
  SnapServer.quickHttpServe (SnapCore.ifTop handleTop)
```

The `handleTop` handler will take care of a GET request to display the
`index.html` file and a POST request to upload files (that is, to handle
the form submission):

```haskell
handleTop :: Snap ()
handleTop =
  SnapCore.method GET handleTop'
    <|> SnapCore.method POST handleFilesUpload
```

The GET request is handled by a function called `handleTop'` that gets
the route for the `index.html` file using the `Paths` module and serves
it:

```haskell
handleTop' :: Snap ()
handleTop' = do
  indexHtml <- MonadIO.liftIO (Paths.getDataFileName "index.html")
  SnapFileServe.serveFile indexHtml
```

The POST request is handled by the `handleFilesUpload` function, which
calls a function named `handleFilesUpload'` and writes the response
(either error messages or the contents of the uploaded files):

```haskell
handleFilesUpload :: Snap ()
handleFilesUpload = do
  eitherFilesUpload <- handleFilesUpload'
  case Either.partitionEithers eitherFilesUpload of
    ([], contents) ->
      SnapCore.writeBS (ByteStringChar8.unlines contents)
    (errors, _) -> do
      SnapCore.modifyResponse
        (SnapCore.setResponseStatus 400 "Bad Request")
      SnapCore.writeText (Text.unlines errors)
```

The `handleFilesUpload'` function uses Snap's
[`handleFileUploads`](https://hackage.haskell.org/package/snap-core-1.0.0.0/docs/Snap-Util-FileUploads.html#v:handleFileUploads)
function, which reads uploaded files to a temporary directory based on
general and per-file upload policies and uses a given handler to
actually do something with the files. Here, we choose to return a list
of either a `Text` (an error) or a `ByteString` (the contents of an
uploaded file):

```haskell
handleFilesUpload' :: Snap [Either Text ByteString]
handleFilesUpload' = do
  temporaryDirectory <- MonadIO.liftIO Directory.getTemporaryDirectory
  SnapFileUploads.handleFileUploads
    temporaryDirectory
    uploadPolicy
    partUploadPolicy
    handleFileRead
```

As temporary directory, we'll simply use the system's temporary
directory.

The general upload policy allows us to specify things like the maximum
number of form inputs and the minimum upload rate. Snap provides a
default upload policy, which is good enough for our application:

```haskell
uploadPolicy :: UploadPolicy
uploadPolicy =
  SnapFileUploads.defaultUploadPolicy
```

With the part or per-file upload policy, we allow or disallow each
submitted file. Given a
[`PartInfo`](https://hackage.haskell.org/package/snap-core-1.0.0.0/docs/Snap-Util-FileUploads.html#t:PartInfo),
we either disallow a file to be uploaded or allow it with a maximum
size. A value of `PartInfo` gives us information such as the content
type of the file and the name of the file (which is optional and will be
`Nothing` if we submit no files):

```haskell
partUploadPolicy
  :: PartInfo
  -> PartUploadPolicy
partUploadPolicy partInfo =
  if (SnapFileUploads.partContentType partInfo == "text/x-haskell"
       || SnapFileUploads.partContentType partInfo == "text/x-literate-haskell")
       && Maybe.isJust (SnapFileUploads.partFileName partInfo)
     then
       SnapFileUploads.allowWithMaximumSize maximumSize
     else
       SnapFileUploads.disallow
```

We only allow Haskell files (`.hs` or `.lhs`) with a name (no empty
submissions) with a maximum size of one megabyte:

```haskell
maximumSize :: Int64
maximumSize =
  1 * megabyte
  where
    megabyte =
      2 ^ (20 :: Int)
```

Note that this part upload policy is not very precise. We could rename a
`turbulent-sniffle.pdf` file to `turbulent-sniffle.hs` and upload it.
Also, the content type of a Haskell file could be submitted as
`application/octet-stream`, which our upload policy would reject.

At last, we can implement the `handleFileRead` and actually do something
with the uploaded files. This handler takes a `PartInfo` and a file path
(or a policy violation) and returns either an error or the contents of
an uploaded file. We already used the `PartInfo` to allow or disallow
the file, so we ignore it here:

```haskell
handleFileRead
  :: PartInfo
  -> Either PolicyViolationException FilePath
  -> IO (Either Text ByteString)
handleFileRead _ eitherFile =
  case eitherFile of
    Left policyViolation ->
      return (Left (SnapFileUploads.policyViolationExceptionReason policyViolation))
    Right file ->
      fmap Right (ByteStringChar8.readFile file)
```
