module Hello.HelloGoldenSpec where

import           Test.Hspec                -- Test
import           Test.Hspec.Golden         -- Golden Tests
import           HelloWorld                -- SUT


spec :: Spec
spec =
    describe "sayHi" $
      it "returns Hello Golden Testers string" $
        defaultGolden "hello" (sayHi "John Doe")
