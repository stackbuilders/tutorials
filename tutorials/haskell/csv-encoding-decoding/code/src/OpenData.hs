{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenData
  ( Item(..)
  , ItemType(..)
  )
  where

-- bytestring
import Data.ByteString.Lazy (ByteString)

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava


-- |
--
--

data ItemType = Country | Other
  deriving (Eq, Show)


-- |
--
--

instance FromField ItemType where
  parseField "International Country" = pure Country
  parseField "International Regional" = pure Other
  parseField "Other State related" = pure Other
  parseField "US City or County" = pure Other
  parseField "US State" = pure Other
  parseField _ = pure Other


-- |
--
--

instance ToField ItemType where
  toField Country = "International Country"
  toField Other = "Other"


-- |
--
--

data Item =
  Item
    { itemName :: ByteString
    , itemLink :: ByteString
    , itemType :: ItemType
    }
  deriving (Eq, Show)


-- |
--
--

instance DefaultOrdered Item where
  headerOrder _ =
    Cassava.header
      [ "Item"
      , "Link"
      , "Type"
      ]


-- |
--
--

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> m .: "Item"
      <*> m .: "Link"
      <*> m .: "Type"


-- |
--
--

instance ToNamedRecord Item where
  toNamedRecord Item{..} =
    Cassava.namedRecord
      [ "Item" .= itemName
      , "Link" .= itemLink
      , "Type" .= itemType
      ]
