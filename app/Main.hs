{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------
-- |
--
----------------------------------------------------------------------

module Main
  ( main
  )
  where

-- base
import Data.Monoid ((<>))
import Data.List (isSuffixOf)
import System.Environment
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)
import Text.Read (readMaybe)

-- filepath
import System.FilePath

-- hakyll
import Hakyll


-- |
--
--

main :: IO ()
main =
  getEnvironment >>= (hakyllWith configuration . rules)


-- |
--
--

configuration :: Configuration
configuration =
  defaultConfiguration
    { ignoreFile  = ignoreFile'
    , previewPort = 4000
    }
  where
    ignoreFile' path =
      ignoreFile defaultConfiguration path
        || takeFileName path `elem` ["makefile", "stack.yaml"]

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

-- |
--
--

rules :: [(String, String)] -> Rules ()
rules env = do
  let commonCtx = commonContext env
  let tutorialCtx = tutorialContext env

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll "tutorials/haskell/*/*.md"
      let
        archiveContext =
          listField "tutorials" tutorialCtx (return tutorials)
            <> constField "title" "Archives"
            <> commonCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveContext
        >>= loadAndApplyTemplate "templates/default.html" archiveContext
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "tutorials/index.html" $ do
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll "tutorials/haskell/*/*.md"
      let
        indexContext =
          listField "tutorials" tutorialCtx (return tutorials)
            <> constField "title" "Home"
            <> commonCtx

      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "tutorials/stylesheets/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" (compile templateCompiler)

  match "tutorials/haskell/*/*.md" $ do
    let
      tutorialRoute i = takeDirectory p </> "index.html"
        where p = toFilePath i
    route (customRoute tutorialRoute)
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/tutorial.html" tutorialCtx
        >>= loadAndApplyTemplate "templates/default.html" tutorialCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "tutorials/haskell/*/*.png" $ do
    route idRoute
    compile copyFileCompiler

  create ["tutorials/sitemap.xml"] $ do
    route   idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "tutorials/haskell/*/*.md"
      let allPosts = return posts
      let sitemapCtx = listField "entries" tutorialCtx allPosts

      makeItem ""
       >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
       >>= cleanIndexHtmls

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

data EnvType = Staging | Production deriving (Show, Read)

commonContext :: [(String, String)] -> Context String
commonContext env =
  let
    host = fromMaybe "" $ lookup "SITE_ROOT_URL" env
  in
    constField "host" host
      <> defaultContext

tutorialContext :: [(String, String)] -> Context String
tutorialContext env = libs
  <> dateField "updated" "%B %e, %Y"
  <> dateField "published" "%B %e, %Y"
  <> commonContext env
  where
    libs = listFieldWith "libs" libraryContext $ \item -> do
      libraries <- getMetadataField' (itemIdentifier item) "libraries"
      mapM makeItem (words libraries)
    libraryContext = field "lib" (return . itemBody)
