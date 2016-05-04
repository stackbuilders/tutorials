{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------
-- |
--
----------------------------------------------------------------------

module Main
  ( main
  )
  where

-- filepath
import System.FilePath

-- hakyll
import Hakyll


-- |
--
--

main :: IO ()
main =
  hakyllWith configuration rules


-- |
--
--

configuration :: Configuration
configuration =
  defaultConfiguration
    { ignoreFile = ignoreFile'
    }
  where
    ignoreFile' path =
      ignoreFile defaultConfiguration path
        || takeFileName path `elem` ["makefile", "stack.yaml"]


-- |
--
--

rules :: Rules ()
rules = do
  match "stylesheets/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" (compile templateCompiler)

  match "tutorials/haskell/*/*.md" $ do
    route (setExtension "html")
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/tutorial.html" tutorialContext
        >>= loadAndApplyTemplate "templates/default.html" tutorialContext
        >>= relativizeUrls


-- |
--
--

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedAuthorEmail = ""
    , feedAuthorName = ""
    , feedDescription = ""
    , feedRoot = ""
    , feedTitle = ""
    }


-- |
--
--

tutorialContext :: Context String
tutorialContext =
  dateField "published" "%B %e, %Y"
    `mappend` dateField "updated" "%B %e, %Y"
    `mappend` defaultContext
