module Hello.HelloGoldenSpec where

import           Test.Hspec
import           Test.Hspec.Golden
import           HelloWorld


spec :: Spec
spec =
    describe "sayHi" $
    it "shows a Hello Golden Testers string" $
    defaultGolden "hello" sayHi