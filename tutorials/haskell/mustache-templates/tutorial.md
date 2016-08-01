---
title: Mustache templates in Haskell
published: 2016-07-28
ghc: 7.10.3
lts: 6.9
libraries: stache-0.1.4
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
Stack Builders. We will show why this package seems to be clearer and easier
to use than the others.

The tutorial should be comprehensible for Haskell beginners and does not
assume knowledge of any advanced Haskell-concepts.

## Available libraries

Let's list the libraries available to work with Mustache templates:

* [`hastache`](https://hackage.haskell.org/package/hastache) is the oldest
  (first released in 2011) and now deprecated library in this list. Those
  who used the library will probably agree that it has not the clearest API
  where a lot of wrapping is necessary and even dictionaries are represented
  as wrapped functions. Generics can be used to alleviate the situation,
  using yet another wrapper called `mkGenericContext`. The library produces
  `ByteString`s.

* [`mustache`](https://hackage.haskell.org/package/mustache) is the official
  successor of `hastache`, but it again makes simple things complex using
  Aeson's `Value` (good) and at the same time introducing its own `Value`
  type (with conflicting names of constructors and naturally not so numerous
  instances). Source code is filled with Unicode symbols (using packages
  like `base-unicode-symbols`, etc.), so it's not easy to edit it if you
  want to contribute to the package. Here and there we could see things like
  `escapeXMLText = T.pack . escapeXML . T.unpack`, `escapeXML :: String ->
  String`, missing package version bounds for dependencies, and generally
  hard-to-read code.

* [`stache`](https://hackage.haskell.org/package/stache) is an alternative
  implementation that conforms to official Mustache spec and passes the
  official test suite. Its API consist of only 4 functions (3 to compile
  templates and one to render them using any instance of `ToJSON` as source
  of data to interpolate), plus Template Haskell helpers to compile/validate
  templates at compile time. `stache` is a bit slower then `mustache`, but
  instead [its source code](https://github.com/stackbuilders/stache) is more
  readable, which seems to be more important than marginal performance
  improvements. Parser is written with Megaparsec 5. The package uses
  `Data.Text.Lazy.Builder` under the hood and produces lazy `Text`.

The main motivation for developing `stache` was the desire to expose more
minimal API and use Aeson's instances directly for value interpolation, as
well as the desire to use Megaparsec instead of Parsec for parsing.
Initially we wanted to contribute to the existing `mustache` library, but
then realized that changes we want to implement are too cardinal and we are
better off writing our own package.

## Compiling templates

Let's see how to compile templates because that's where you usually start.
But first, let's take a look at types that will show up here and there in
the rest of the tutorial:

* `PName` stands for “partial's name”. “Partials” are other templates that
  are inserted into actual template you're rendering. `PName` is a wrapper
  around `Text` and defined to make it harder to mix it up with other
  textual values. If you enable the `OverloadedStrings` extension, you can
  write `PName`s just as normal strings.

* `Node` is a piece of template. The whole template body is represented as
  `[Node]` and that's all you need to know unless you plan to manipulate
  parsed representation of template, which is also easy, but not in the
  scope of this tutorial.

### Three ways to compile a template

A `Template` is actually a collection of templates with one of them
selected:

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

When working with a `Template`, it's only possible to use partials  that are
in `templateCache`, thus the main difference between compiling functions is
where you get your template(s) and what you will have in `templateCache`.
Thus there are three different ways to get a `Template`:

1. From lazy `Text` with `compileMustacheText`. The function takes a `PName`
   and actual template source. The function returns `Either` parse error or
   `Template` with cache consisting of only one `Template`.

2. From a single `File` with `compileMustacheFile`. This one only takes path
   to Mustache file the resulting template won't be able to use partials
   unless you combine several templates into one (see below).

3. From a directory with `compileMustacheDir`. This function reads all
   templates (files ending with `.mustache` extension) and puts them into
   cache, selecting one of them as main (you specify which). The resulting
   `Template` can use partials that were present in that directory.

So to use partials we need to use `compileMustacheDir` *or* combine several
templates into one using `(<>)` method of `Semigroup` typeclass. `Template`
is a `Semigroup`, which means you can always to two `Template`s and get
their combination which will also be a `Template`. This is incredible
property, one of numerous examples how Haskell keeps systems “flat”.

Let's start build practical stuff to see how things play together. As a
practical example, we will be generating a
[CUE sheet](https://en.wikipedia.org/wiki/Cue_sheet_(computing)). First of
all we need some imports:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics
import Text.Mustache
```

Then we will need to represent data somehow, for that let's define the
following:

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
for many other problem domains. We also have used *generics* to quickly
define `ToJSON` instance for the data types. (**About deriving…**)

Now we can experiment with template compilation. We will have two templates:
`main.mustache` and `file.mustache` (we could do with just one, but then I
wouldn't have a good example how to work with partials).

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

↑ Here we have typical header of a CUE file. The `{{> file }}` part inserts
a partial which we can write this way (`file.mustache` file):

```mustache
FILE "{{& fiFileName }}" WAVE
  TRACK {{& fiIndex }} AUDIO
    TITLE "{{& fiTitle }}"
    PERFORMER "{{& fiPerformer }}"
    INDEX 00 {{& fiIndex00 }}
```

Given that the template files are stored in `data/` directory, the following
code will produce a `Template` with both of them stored in the cache (so we
can use one from another, like we indend to do):

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

The TH helpers live in the `Text.Mustache.Compile.TH` correspond precisely
to the three normal functions we already know. Let's play with a template
Haskell helper right in your editing environment.

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

For your purposes let's use TH version of `compileMustacheDir`, now if
nay template in that dir is malformed, the program won't compile telling us
about the error.

```haskell
main :: IO ()
main = do
  let template = $(TH.compileMustacheDir "main" "data/")
  print template
```

Note that the TH helpers only work with GHC 8 for now. (**More here…**)

## Rendering

There is just one function that you need to know to render your template —
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
      , fiIndex00 = "00:00:00"
      }
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

As simple as that, if you run the program, our CUE file looks good:

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
```

How does the magic work?

But fear not dear reader.

Also: how to do it manually.

## Conclusion

I would not use anything but `stache` to work with Mustache templates in
Haskell even if they paid me.
