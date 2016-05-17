{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------
-- |
--
----------------------------------------------------------------------

module Main
  ( main
  )
  where

import Data.Monoid ((<>))

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
            <> constField "title" "Archives"
            <> commonContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveContext
        >>= loadAndApplyTemplate "templates/default.html" archiveContext
        >>= relativizeUrls

  match "tutorials/index.html" $ do
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll "tutorials/haskell/*/*.md"
      let
        indexContext =
          listField "tutorials" tutorialContext (return tutorials)
            <> constField "title" "Home"
            <> commonContext

      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
        >>= relativizeUrls

  match "stylesheets/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" (compile templateCompiler)

  match "tutorials/haskell/*/*.md" $ do
    route (customRoute (\i -> takeDirectory (toFilePath i) <> ".html"))
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/tutorial.html" tutorialContext
        >>= loadAndApplyTemplate "templates/default.html" tutorialContext
        >>= relativizeUrls

  match "tutorials/haskell/*/*.png" $ do
    route idRoute
    compile copyFileCompiler

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

commonContext :: Context String
commonContext =
  constField "base_url" "//www.stackbuilders.com"
    `mappend` defaultContext

tutorialContext :: Context String
tutorialContext = libs
  <> dateField "updated" "%B %e, %Y"
  <> dateField "published" "%B %e, %Y"
  <> commonContext
  where
    libs = listFieldWith "libs" libraryContext $ \item -> do
      libraries <- getMetadataField' (itemIdentifier item) "libraries"
      mapM makeItem (words libraries)
    libraryContext = field "lib" (return . itemBody)
