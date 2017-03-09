module Config where

import Database.PostgreSQL.Simple

import System.Environment.Compat

readConfig :: IO ConnectInfo
readConfig =
  ConnectInfo
    <$> getEnv "DB_HOST"
    <*> fmap read (getEnv "DB_PORT")
    <*> getEnv "DB_USER"
    <*> getEnv "DB_PASS"
    <*> getEnv "DB_NAME"
