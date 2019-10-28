module FizzBuzz.FizzBuzzGoldenSpec where


import           Test.Hspec
import           Test.Hspec.Golden
import           FIZZBUZZ.FizzBuzz

spec :: Spec
spec = 
    describe "fizzBuzz" $
      it "Turns 3 multiples to fizz and 5 multiples to buzz" $
        defaultGolden "fizzbuzz" (show $ fizzBuzz [1,2,3,4,5,11,12,13,14,15])
