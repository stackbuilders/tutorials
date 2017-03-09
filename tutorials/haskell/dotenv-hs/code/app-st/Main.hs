module Main where

import Control.Monad

import Config
import Connection
import User

main :: IO ()
main = do
  conn <- readConfig >>= getConnection
  users <- findAllUsers conn
  forM_ users print
