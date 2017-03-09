{-# LANGUAGE OverloadedStrings #-}

module User.UserSpec (spec) where

import Control.Exception.Base

import Data.Time.Calendar

import Database.PostgreSQL.Simple

import Test.Hspec

import Config
import Connection
import User

spec :: Spec
spec =
  describe "findAllUsers" $
    it "finds all the users" $ do
      config <- readConfig
      conn <- getConnection config
      allUsers <- findAllUsers conn
      allUsers `shouldBe` [ User {id_ = 1, name = "User 1", lastname = "User 1", birthDate = ModifiedJulianDay 47892 }
                          , User {id_ = 2, name = "User 2", lastname = "User 2", birthDate = ModifiedJulianDay 47892 }
                          , User {id_ = 3, name = "User 3", lastname = "User 3", birthDate = ModifiedJulianDay 47892 }
                          , User {id_ = 4, name = "User 4", lastname = "User 4", birthDate = ModifiedJulianDay 47892 }
                          ]
