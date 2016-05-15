{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

----------------------------------------------------------------------
-- |
-- Module      : OpenData
--
--
--
----------------------------------------------------------------------

module OpenData
  ( Item(..)
  , ItemType(..)
  , decodeItems
  , decodeItemsFromFile
  , encodeItems
  , encodeItemsToFile
  , filterCountryItems
  , itemHeader
  , japanItem
  , japanRecord
  )
  where

-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector


-- |
--
-- An item type.

data ItemType
  = Country
  | Other Text
  deriving (Eq, Show)


-- |
--
-- Parse an item type from a CSV field.

instance FromField ItemType where
  parseField "International Country" =
    pure Country

  parseField otherType =
    Other <$> parseField otherType


-- |
--
-- Convert an item type to a CSV field.

instance ToField ItemType where
  toField Country =
    "International Country"

  toField (Other otherType) =
    toField otherType


-- |
--
-- An item.

data Item =
  Item
    { itemName :: Text
    , itemLink :: Text
    , itemType :: ItemType
    }
  deriving (Eq, Show)


-- |
--
--

isCountryItem
  :: Item
  -> Bool
isCountryItem =
  (==) Country . itemType


-- |
--
--

filterCountryItems
  :: Vector Item
  -> Vector Item
filterCountryItems =
  Vector.filter isCountryItem


-- |
--
--

itemHeader :: Header
itemHeader =
  Vector.fromList
    [ "Item"
    , "Link"
    , "Type"
    ]


-- |
--
--

japanItem :: Item
japanItem =
  Item
    { itemName = "Japan"
    , itemLink = "http://www.data.go.jp/"
    , itemType = Country
    }


-- |
--
--

japanRecord :: ByteString
japanRecord =
  "Japan,http://www.data.go.jp/,International Country"


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
-- Parse an item from a CSV record.

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> fmap Text.decodeLatin1 (m .: "Item")
      <*> m .: "Link"
      <*> m .: "Type"


-- |
--
-- Convert an item to a CSV record.

instance ToNamedRecord Item where
  toNamedRecord Item{..} =
    Cassava.namedRecord
      [ "Item" .= itemName
      , "Link" .= itemLink
      , "Type" .= itemType
      ]


-- |
--
-- Decode a vector of items from CSV.

decodeItems
  :: ByteString
  -> Either String (Vector Item)
decodeItems =
  fmap snd . Cassava.decodeByName


-- |
--
-- Encode a vector of items to CSV.

encodeItems
  :: Vector Item
  -> ByteString
encodeItems =
  Cassava.encodeDefaultOrderedByName . Foldable.toList


-- |
--
-- Decode a vector of items from a CSV file.

decodeItemsFromFile
  :: FilePath
  -> IO (Either String (Vector Item))
decodeItemsFromFile filePath = do
  catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeItems


-- |
--
-- Encode a vector of items to a CSV file.

encodeItemsToFile
  :: FilePath
  -> Vector Item
  -> IO (Either String ())
encodeItemsToFile filePath =
  catchShowIO . ByteString.writeFile filePath . encodeItems


-- |
--
-- Run an action. If an IO exception is raised, show it.

catchShowIO
  :: IO a
  -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show
