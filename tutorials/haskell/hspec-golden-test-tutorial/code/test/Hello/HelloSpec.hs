module Hello.HelloSpec where

import HelloWorld
import Test.Hspec

spec :: Spec
spec = do
    describe "sayHi" $ 
        it "shows a Hello Golden Testers string" $
            sayHi `shouldBe` "Hello Golden Testers"