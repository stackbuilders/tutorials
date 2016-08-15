---
title: Mustache templates in Haskell
published: 2016-08-17
ghc: 8.0.1
nightly: 2016-08-15
libraries: stache-0.1.6
language: haskell
author: Mark Karpov
author-name: Mark Karpov
description: In this tutorial we are going to use the Stache Haskell package developed by Stack Builders to work with Mustache templates — a popular logic-less template format.
---

[Mustache templates](https://mustache.github.io/) are popular logic-less
templates that can be used in web development as well as in any other domain
where interpolation of data into some sort of template is needed.
“Logic-less templates” means that the templates themselves do not have any
logic such as conditionals (although it's possible to choose how to
represent empty collections for example) and thus it's much harder to shoot
yourself in the foot when concerns of data processing and representation are
separated.

Libraries supporting working with Mustache templates are available for many
languages, including Haskell (there are several of them, actually). In this
tutorial we will be using the
[`stache`](https://hackage.haskell.org/package/stache) package developed by
Stack Builders and show why this package is clear and easy to use.

The tutorial should be comprehensible for Haskell beginners and does not
assume knowledge of any advanced Haskell-concepts.

## Available libraries

Let's see which libraries are available to work with Mustache templates:

* [`hastache`](https://hackage.haskell.org/package/hastache) is the oldest
  (first released in 2011) and now deprecated library in this list. Those
  who used the library will probably agree that it doesn't have the clearest API
  where a lot of wrapping is necessary and dictionaries are represented as
  wrapped functions. Generics can be used to alleviate the situation, using
  yet another wrapper called `mkGenericContext`.

* [`mustache`](https://hackage.haskell.org/package/mustache) is the official
  successor of `hastache`, but it again makes simple things complex using
  Aeson's `Value` (good) and at the same time introducing its own `Value`
  type (with conflicting names of constructors and naturally not so numerous
  instances). Source code is filled with Unicode symbols (using packages
  like `base-unicode-symbols`, etc.), so it's not easy to edit it if you
  want to contribute to the package.

* [`stache`](https://hackage.haskell.org/package/stache) is an alternative
  implementation that conforms to official Mustache spec and passes the
  official test suite. Its API consists of only 4 functions (3 to compile
  templates and one to render them using any instance of `ToJSON` as source
  of data to interpolate), plus Template Haskell helpers to compile/validate
  templates at compile time. The parser is written with Megaparsec 5. The
  package uses `Data.Text.Lazy.Builder` under the hood and produces lazy
  `Text`.

The main motivation for developing `stache` was the desire to expose a more
minimal API and use Aeson's instances directly for value interpolation, as
well as the desire to use Megaparsec instead of Parsec for parsing.
Initially we wanted to contribute to the existing `mustache` library, but
then realized that the changes we wanted to implement were too cardinal and
we were better off writing our own package.

One feature that is not supported by `stache` is lambdas. The feature is
marked as optional in the spec and can be emulated via processing of parsed
template representation. The decision to drop lambdas is intentional, for
the sake of simplicity and better integration with Aeson.

## Compiling templates

Let's see how to compile templates because that's where you usually start.
But first, let's take a look at types that will show up here and there in
the rest of the tutorial:

* `PName` stands for “partial's name”. “Partials” are other templates that
  are inserted into the actual template you're rendering. `PName` is a
  wrapper around `Text` and defined to make it harder to mix it up with
  other textual values. If you enable the `OverloadedStrings` extension, you
  can write `PName`s just as normal `String`s.

* `Node` is a piece of template. The whole template body is represented as
  `[Node]` and that's all you need to know unless you plan to manipulate
  parsed representation of template, which is also easy, but not in the
  scope of this tutorial.

### Three ways to compile a template

A `Template` is actually a collection of templates (partials) with one of
them selected:

```haskell
-- | Mustache template as name of “top-level” template and a collection of
-- all available templates (partials).
--
-- 'Template' is a 'Semigroup'. This means that you can combine 'Template's
-- (and their caches) using the ('<>') operator, the resulting 'Template'
-- will have the same currently selected template as the left one. Union of
-- caches is also left-biased.

data Template = Template
  { templateActual :: PName
    -- ^ Name of currently “selected” template (top-level one).
  , templateCache  :: Map PName [Node]
    -- ^ Collection of all templates that are available for interpolation
    -- (as partials). The top-level one is also contained here and the
    -- “focus” can be switched easily by modifying 'templateActual'.
  } deriving (Eq, Ord, Show, Data, Typeable, Generic)
```

When working with a `Template`, it's only possible to use partials that are
in `templateCache`, thus the main difference between compiling functions is
where you get your template(s) and what you will have in `templateCache`.
So, there are three different ways to get a `Template`:

1. From lazy `Text` with `compileMustacheText`. The function takes a `PName`
   and actual template source. The function returns `Either` parse error or
   `Template` with cache consisting of only one `Template`.

2. From a single `File` with `compileMustacheFile`. This one only takes the
   path to Mustache file. Note that the resulting template won't be able to
   use partials unless you combine several templates into one (see below).

3. From a directory with `compileMustacheDir`. This function reads all
   templates (files ending with `.mustache` extension) and puts them into
   the cache, selecting one of them as main (you specify which). The
   resulting `Template` can use partials that were present in that
   directory.

So to have partials, we need to use `compileMustacheDir` **or** combine
several templates into one using `(<>)` method of `Semigroup` type class.
Being an instance of the `Semigroup` type class means that you can always
combine two `Template`s and get their combination which will also be a
`Template`. This is an incredible property, one of numerous examples how
Haskell keeps systems “flat”.

Let's start building practical stuff to see how things play together. As an
example of a task that could involve Mustache templates, we will be
generating a
[CUE sheet](https://en.wikipedia.org/wiki/Cue_sheet_(computing)). First of
all, we're going to need some imports:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics
import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH
import qualified Data.Text.Lazy.IO as TIO
```

Then we will need to represent data somehow. For that, let's define the
following data records:

```haskell
data Release = Release
  { reGenre     :: Text
  , reDate      :: Text
  , reDiscId    :: Text
  , reComment   :: Text
  , rePerformer :: Text
  , reTitle     :: Text
  , reFiles     :: [File]
  } deriving (Eq, Show, Generic)

instance ToJSON Release

data File = File
  { fiFileName  :: Text
  , fiIndex     :: Int
  , fiTitle     :: Text
  , fiPerformer :: Text
  , fiIndex00   :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON File
```

Here we have defined two records with various fields, just like we would do
for other domains. We also have used
[*generics*](https://wiki.haskell.org/GHC.Generics) to quickly define
`ToJSON` instance for the data types. You can read more about generics, but
the technique in a nutshell is simple: the compiler can derive `Generic`
instance for your datatype (with `DeriveGeneric` extension enabled) which
describes “structure” of your data, and then you can use that information to
(again automatically) derive instance of `ToJSON` type class.

Now we can experiment with template compilation. We will have two templates:
`main.mustache` and `file.mustache` (we could do with just one, but then I
wouldn't have a good example of how to work with partials).

The `main.mustache` looks like this:

```mustache
REM GENRE "{{& reGenre }}"
REM DATE "{{& reDate }}"
REM DISCID "{{& reDiscId }}"
REM COMMENT "{{& reComment }}"
PERFORMER "{{& rePerformer }}"
TITLE "{{& reTitle }}"
{{# reFiles }}
{{> file }}
{{/ reFiles }}
```

Here we have the typical layout of a CUE file header. The `{{> file }}` part
inserts a partial which we can write this way (`file.mustache` file):

```mustache
FILE "{{& fiFileName }}" WAVE
  TRACK {{& fiIndex }} AUDIO
    TITLE "{{& fiTitle }}"
    PERFORMER "{{& fiPerformer }}"
    INDEX 00 {{& fiIndex00 }}
```

Given that the template files are stored in `data/` directory, the following
code will produce a `Template` with both of them stored in the cache (so we
can use one from another, like we intend to do):

```haskell
main :: IO ()
main = do
  mainTemplate <- compileMustacheFile "data/main.mustache" -- (1)
  fileTemplate <- compileMustacheFile "data/file.mustache" -- (2)
  let template = mainTemplate <> fileTemplate -- (3)
  print template -- (4)
```

What's going on?

1. We load `Template` from `data/main.mustache`.
2. We load `Template` from `data/file.mustache`.
3. We combine caches of the two templates and get another template which is
   just like `main` but with `file` added to its cache.
4. We `print` the result because we don't know any better yet.

Another way to do the same is to use `compileMustacheDir`:

```haskell
main :: IO ()
main = do
  template <- compileMustacheDir "main" "data/"
  print template
```

Easy.

### Template Haskell helpers

If you do not plan to change templates “on the fly” (without re-compiling
your application), you may like Template Haskell (TH) helpers that compile
Mustache templates at compile time and ensure that your templates are valid.
If some template is not valid, your program just won't compile.

The TH helpers live in the `Text.Mustache.Compile.TH` module and correspond
precisely to the three normal functions we already know. Let's play with a
template Haskell helper right in your editing environment.

```haskell
main :: IO ()
main = do
  let template = $(TH.compileMustacheText "main" "")
  print template
```

Try to change the last argument of the `TH.compileMustacheText` function and
if you have “on the fly” compilation and highlighting, you will be able to
observe error messages any time your input is not a valid Mustache template.
Invalid templates just won't compile!

For our purposes it's better yet to use the TH version of
`compileMustacheDir`. Now if any template in the specified directory is
malformed, the program won't compile telling us about the error and where it
occurred.

```haskell
main :: IO ()
main = do
  let template = $(TH.compileMustacheDir "main" "data/")
  print template
```

Note that the TH helpers only work with GHC 8 for now.

## Rendering

There is just one function that you need to render your template —
`renderMustache`. Its type signature looks like this:

```haskell
renderMustache :: Template -> Value -> Text
```

First you feed it with a `Template` (we just discovered how to get these),
then you need a source of values to interpolate. For that we need a `Value`
which comes from Aeson — a library for working with JSON in Haskell. Why to
do it this way? Well, Mustache templates originate from languages with duck
typing, so to feed values into a Mustache template, we need to categorize
them to understand how they should interact with the template we are
rendering. Aeson's `Value` is exactly a type that is close to what you have
in Ruby (and of course in JavaScript), and since Aeson is a very popular
package, we already know how to convert pretty much any type to `Value`.
Isn't that cool?

To interpolate something we need to have it first. Let's put together some
data:

```haskell
release :: Release
release = Release
  { reGenre  = "Ambient"
  , reDate   = "1980"
  , reDiscId = "380B7905"
  , reComment = "ExactAudioCopy v0.95b4"
  , rePerformer = "Laraaji"
  , reTitle = "Ambient 3 Day Of Radiance"
  , reFiles =
    [ File
      { fiFileName = "01 - The Dance #1.wav"
      , fiIndex = 1
      , fiTitle = "The Dance #1"
      , fiPerformer = "Laraaji"
      , fiIndex00 = "00:00:00" }
    , File
      { fiFileName = "02 - The Dance #2.wav"
      , fiIndex = 2
      , fiTitle = "The Dance #2"
      , fiPerformer = "Laraaji"
      , fiIndex00 = "09:06:10" }
    ]
  }
```

Just use the `renderMustache` function now with `toJSON` (which transforms
instances of `ToJSON` to `Value`):

```
main :: IO ()
main = do
  let template = $(TH.compileMustacheDir "main" "data/")
  TIO.putStrLn $ renderMustache template (toJSON release)
```

As simple as that, if you run the program, you will see that our CUE file
looks good:

```
REM GENRE "Ambient"
REM DATE "1980"
REM DISCID "380B7905"
REM COMMENT "ExactAudioCopy v0.95b4"
PERFORMER "Laraaji"
TITLE "Ambient 3 Day Of Radiance"
FILE "01 - The Dance #1.wav" WAVE
  TRACK 1 AUDIO
    TITLE "The Dance #1"
    PERFORMER "Laraaji"
    INDEX 00 00:00:00
FILE "02 - The Dance #2.wav" WAVE
  TRACK 2 AUDIO
    TITLE "The Dance #2"
    PERFORMER "Laraaji"
    INDEX 00 09:06:10
```

How does the magic work? What to do if you want to construct a `Value`
manually? Simply put, `toJSON` does not do anything you can't do yourself.
Records are transformed into JSON `Object`s, lists and vectors and turned
into `Array`s, etc. Feeding the `renderMustache` without `toJSON` is just as
easy (consult
[Aeson documentation](https://hackage.haskell.org/package/aeson) for more
information), but the most common thing you will want to do is to create a
custom dictionary that will be your “context”. This is done with help of the
`object` and `(.=)` functions:

```haskell
main :: IO ()
main = do
  let template = $(TH.compileMustacheDir "main" "data/")
  TIO.putStrLn $ renderMustache template $ object
    [ "reGenre"   .= "My Genre"
    , "reDate"    .= "2016"
    , "reDiscId"  .= "blahblah"
    -- …
    ]
```

It just couldn't be easier.

## Conclusion

`stache` seems to do its job just fine so far. We migrated some code from
`hastache` and were surprised just how simpler the code looked with
`stache`. It's also nice to be able to check your templates at compile time,
like Shakespearean templates do. So in conclusion I have to say that I
wouldn't use anything but `stache` to work with Mustache.
