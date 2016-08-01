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

main :: IO ()
main = do
  let template = $(TH.compileMustacheDir "main" "data/")
  TIO.putStrLn $ renderMustache template (toJSON release)
