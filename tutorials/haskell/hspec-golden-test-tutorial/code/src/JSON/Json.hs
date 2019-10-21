{-# LANGUAGE DeriveGeneric #-}

module JSON.Json where

import            Data.Aeson                                  (ToJSON, encode) 
import            GHC.Generics                                (Generic)
import            Data.ByteString.Lazy                        (ByteString)
import qualified  Data.ByteString.Lazy    as ByteString

countriesPath :: FilePath
countriesPath = "country.json"

countriesJSON :: IO ByteString
countriesJSON = ByteString.readFile countriesPath


data Country = Country 
  {
    cname      :: String,
    continent :: String,
    ctag       :: Int
  } deriving (Generic, Show)

instance ToJSON Country 


ecuador = Country "Ecuador" "America" 1
germany = Country "Germany" "Europe" 2
japan = Country "Japan" "Asia" 3

countries :: [Country]
countries = [ecuador,germany,japan]

encodeCountries :: [Country] -> ByteString
encodeCountries = encode

