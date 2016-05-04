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
  match "templates/*" (compile templateCompiler)

  match "tutorials/haskell/*/*.md" $ do
    route (setExtension "html")
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/tutorial.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
