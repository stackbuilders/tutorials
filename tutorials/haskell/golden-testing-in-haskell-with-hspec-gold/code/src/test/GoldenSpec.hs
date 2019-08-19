module GoldenSpec where

import Test.Hspec
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Test.Hspec.Golden
import Data.Yaml as Y
import Data.Aeson as A
import Control.Exception (evaluate)

{--
In case our output isn't a String (is a ByteString) so we defined our own
Golden test using the Golden data type!
--}
myGoldenTest :: String -> BL.ByteString -> Golden BL.ByteString
myGoldenTest name actualOutput =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = BL.writeFile,
    readFromFile = BL.readFile,
    testName = name,
    directory = ".golden"
  }


{--
Here we define our Golden test as follows:
- We read a .yml file that contains an ansible role
- We decode the string read by Haskell that we read form the .yml file
- 
--}
spec :: Spec
spec = do
    describe "conversion" $ do
      content <- runIO $ B.readFile "test/fixtures/test.yml"
      let eYaml = Y.decodeEither' content :: Either Y.ParseException Y.Value --Parses to a generic yml type.
          Right json = A.encode <$> eYaml --Parses the data type to a json structure.
      it "transforms a YAML file contents to JSON" $ do
        myGoldenTest "conversion" json --Generates the golden test.