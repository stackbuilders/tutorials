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
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll "tutorials/haskell/*/*.md"
      let
        archiveContext =
          listField "tutorials" tutorialContext (return tutorials)
            `mappend` constField "title" "Archives"
            `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveContext
        >>= loadAndApplyTemplate "templates/default.html" archiveContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll "tutorials/haskell/*/*.md"
      let
        indexContext =
          listField "tutorials" tutorialContext (return tutorials)
            `mappend` constField "title" "Home"
            `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
        >>= relativizeUrls

  match "stylesheets/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" (compile templateCompiler)

  match "tutorials/haskell/*/*.md" $ do
    route (customRoute (\i -> takeDirectory (toFilePath i) `mappend` ".html"))
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
