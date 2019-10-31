{-# LANGUAGE DeriveGeneric #-}

module JSON.Json where

import            Data.Aeson                                  (ToJSON, encode)
import            GHC.Generics                                (Generic)
import            Data.ByteString.Lazy                        (ByteString)



data Country = Country
  {
    cname      :: String,
    continent :: String,
    ctag       :: Int
  } deriving (Generic, Show)

instance ToJSON Country

ecuador = Country "Ecuador" "America" 1
germany = Country "Germany" "Europe" 2
japan = Country "Japan" "Asia" 4

countries :: [Country]
countries = [ecuador,germany,japan]

encodeCountries :: [Country] -> ByteString
encodeCountries = encode
