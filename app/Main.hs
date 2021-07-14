{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Char                  (toLower)
import           Data.List                  (isSuffixOf)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Hakyll
import           System.Environment
import           System.FilePath
import           Text.Jasmine
import           Text.Pandoc

import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = getEnvironment >>= (hakyllWith configuration . rules)

sitePort :: Int
sitePort = 4000

supportedLanguages :: [String]
supportedLanguages = [  -- Name here the directories of the programming languages to be supported
    "elixir"
  , "haskell"
  , "functional-full-stack" -- Haskell backend + PureScript frontend
  , "java"
  , "python"
  , "reason"
  , "elm"
  ]

markdownPattern :: Pattern
markdownPattern = filePattern ["md"]

imagePattern :: Pattern
imagePattern = filePattern ["png", "gif"]

filePattern :: [String] -> Pattern
filePattern extensions = foldl (.||.) (head patterns) (tail patterns)
  where
    patterns = fmap (\(dir, ext) -> fromGlob $ "tutorials/" ++ dir ++ "/*/*." ++ ext) extensionByLanguage
    extensionByLanguage = [(dir, ext) | dir <- supportedLanguages, ext <- extensions]

configuration :: Configuration
configuration =
  defaultConfiguration
    { ignoreFile  = ignoreFile'
    , previewPort = sitePort
    }
  where
    ignoreFile' path =
      ignoreFile defaultConfiguration path
        || takeFileName path `elem` ["makefile", "stack.yaml"]

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern' replacement)
    where
      pattern' = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

rules :: [(String, String)] -> Rules ()
rules env = do
  let commonCtx   = commonContext Blog env
      datedCtx    = datedContext env
      tutorialCtx = tutorialContext env

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll markdownPattern
      let
        title = "Archives"
        archiveContext =
          listField "tutorials" tutorialCtx (return tutorials)
            <> constField "title" title
            <> constField "title-list" title
            <> commonCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveContext
        >>= loadAndApplyTemplate "templates/default.html" archiveContext
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "tutorials/index.html" $ do
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll markdownPattern
      let
        indexContext =
          listField "tutorials" tutorialCtx (return tutorials)
            <> constField "title" "Home"
            <> constField "title-list" ""
            <> commonCtx

      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
        >>= relativizeUrls
        >>= cleanIndexUrls

  match "tutorials/stylesheets/*" $ do
    route idRoute
    compile compressCssCompiler

  match "tutorials/javascripts/*" $ do
    route idRoute
    compile compressJsCompiler

  match "templates/*" (compile templateCompiler)

  match "tutorials/*.png" $ do
    route idRoute
    compile copyFileCompiler

  tags <- buildTags markdownPattern (fromCapture "tutorials/tags/*.html")

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title
                `mappend` constField "title-list" title
                `mappend` listField "tutorials" tutorialCtx (return tutorials)
                `mappend` defaultContext
                `mappend` commonCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match markdownPattern $ do
    let
      tutorialRoute i = takeDirectory p </> "index.html"
        where p = toFilePath i
    route (customRoute tutorialRoute)
    compile $
      tutorialsCompiler
        >>= loadAndApplyTemplate "templates/tutorial.html" (tutorialCtxWithTags env tags)
        >>= loadAndApplyTemplate "templates/default.html" (tutorialCtxWithTags env tags)
        >>= relativizeUrls
        >>= cleanIndexUrls

  match imagePattern $ do
    route idRoute
    compile copyFileCompiler

  create ["tutorials/sitemap.xml"] $ do
    route   idRoute
    compile $ do
      posts <- recentFirst =<< loadAll markdownPattern
      let allPosts = return posts
      let sitemapCtx = listField "entries" tutorialCtx allPosts

      makeItem ("" :: String)
       >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
       >>= cleanIndexHtmls

  let pumpFeedPosts =
        fmap (take 10) . recentFirst =<< loadAll markdownPattern

  create ["tutorials/atom.xml"] $ do
    route idRoute
    compile (pumpFeedPosts >>= renderAtom feedConfiguration datedCtx)

  create ["tutorials/rss.xml"] $ do
    route idRoute
    compile (pumpFeedPosts >>= renderRss feedConfiguration datedCtx)

tutorialCtxWithTags :: [(String, String)] -> Tags -> Context String
tutorialCtxWithTags env tags = do
  let tutorialCtx = tutorialContext env
  tagsField "tags" tags `mappend` tutorialCtx

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle       = "Stack Builders' Tutorials"
    , feedDescription = "Tutorials about tech Stack Builders consider important to promote"
    , feedAuthorName  = "Stack Builders"
    , feedAuthorEmail = "info@stackbuilders.com"
    , feedRoot        = "https://stackbuilders.com"
    }

data Hero
  = Blog
  | Post
  deriving Show

commonContext :: Hero -> [(String, String)] -> Context String
commonContext hero env =
  let
    readEnv d key = fromMaybe d $ lookup key env
    host = readEnv ("//localhost:" ++ show sitePort) "SITE_ROOT_URL"
    protocol = readEnv "http:" "SITE_PROTOCOL"
  in
    constField "host" host
      <> constField "protocol" protocol
      <> constField "hero" (map toLower (show hero))
      <> defaultContext

datedContext :: [(String, String)] -> Context String
datedContext env = dateField "published" "%B %e, %Y" <> commonContext Post env

tutorialContext :: [(String, String)] -> Context String
tutorialContext env = libs <> datedContext env
  where
    libs = listFieldWith "libs" libraryContext $ \item -> do
      libraries <- getMetadataField' (itemIdentifier item) "libraries"
      mapM makeItem (words libraries)
    libraryContext = field "lib" (return . itemBody)

tutorialsCompiler :: Compiler (Item String)
tutorialsCompiler = pandocCompilerWith defaultTutorialsReaderOptions defaultHakyllWriterOptions

defaultTutorialsReaderOptions :: ReaderOptions
defaultTutorialsReaderOptions = defaultHakyllReaderOptions
    { readerSmart = False
    }

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  source <- getResourceString
  return $ itemSetBody (minifyJS source) source
