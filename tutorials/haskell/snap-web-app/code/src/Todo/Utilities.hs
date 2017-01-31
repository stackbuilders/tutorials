module Todo.Utilities
  ( showAsText
  , byteStringToText
  , fromBSTo
  )where

import           Data.ByteString    (ByteString)
import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           Text.Read          (readMaybe)

-- | Returns a Data.Text.Text value from a data
-- type value implementing class Show
showAsText  :: Show a => a
            -> T.Text
showAsText = T.pack . show

-- | Returns a Data.Text.Text value from a ByteString one
byteStringToText  :: ByteString
                  -> T.Text
byteStringToText = E.decodeUtf8

-- | Returns a String value from a ByteString one
byteStringToStr :: ByteString
                -> String
byteStringToStr =  T.unpack . E.decodeUtf8

-- | Returns a value implementing Show class from a ByteString value.
-- It receives a default value in case the converation fails.
-- Notice you have explicitely specify the type of a
fromBSTo  :: Read a => a
          -> ByteString
          -> a
fromBSTo defaultValue bs = fromMaybe defaultValue (readMaybe ( byteStringToStr bs))
