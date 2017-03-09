module Main where

import Control.Monad

import Configuration.Dotenv
import Configuration.Dotenv.Types

import Config
import Connection
import User

config :: Config
config =
  Config
    { configExamplePath = ".env.example"
    , configOverride = True
    , configPath = ".env"
    }

main :: IO ()
main = do
  void $ loadFile config
  conn <- readConfig >>= getConnection
  users <- findAllUsers conn
  forM_ users print

