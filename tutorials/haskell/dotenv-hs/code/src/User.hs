{-# LANGUAGE QuasiQuotes #-}

module User where

import Data.ByteString

import Data.Time.Calendar

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ

data User =
  User
    { id_ :: Integer
    , name :: ByteString
    , lastname :: ByteString
    , birthDate :: Day
    } deriving (Show, Eq)

instance FromRow User where
    fromRow =
      User
        <$> field
        <*> field
        <*> field
        <*> field

queryFindAllUsers :: Query
queryFindAllUsers = [sql|
    SELECT id, name, lastname, birth FROM USERS
  |]

findAllUsers :: Connection -> IO [User]
findAllUsers conn =
  query_ conn queryFindAllUsers

