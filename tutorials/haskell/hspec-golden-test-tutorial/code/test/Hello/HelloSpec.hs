module Hello.HelloSpec where

import HelloWorld
import Test.Hspec

spec :: Spec
spec = do
    describe "sayHi" $ 
        it "shows a Hello Golden Testers string" $
            (sayHi "John Doe") `shouldBe` "Welcome to the Golden Tests tutorial John Doe"
