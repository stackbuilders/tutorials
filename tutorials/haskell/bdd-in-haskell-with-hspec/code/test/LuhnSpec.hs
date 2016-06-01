module LuhnSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Luhn
import Luhn.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "converts a number to a list of digits" $ do
      toDigits 1234567 `shouldBe` [1..7]
      toDigits 2468 `shouldBe` [2,4,6,8]
    context "for natural numbers" $
      it "holds on: x == fromDigits(toDigits x)" $
        property $ \x ->
          x >= 0 ==> x == (fromDigits . toDigits) x
    context "for negative numbers" $
      it "holds on: fromDigits(toDigits x) == 0" $
        property $ \x ->
          x < 0 ==> (fromDigits . toDigits) x == 0
  describe "doubleEveryOther" $
    it "doubles every other number starting from the right" $ do
      doubleEveryOther [1..5] `shouldBe` [1,4,3,8,5]
      doubleEveryOther [1..6] `shouldBe` [2,2,6,4,10,6]
  describe "sumDigits" $ do
    context "when all numbers are less than 10" $
      it "sums the list of integers" $
        sumDigits [1..6] `shouldBe` sum [1..6]
    context "when some numbers are greater or equal to 10" $
      it "sums their digits first before summing the list" $
        sumDigits [2,12,4,14,6,8] `shouldBe` 28
  describe "validate" $
    it "returns True if number is valid, False otherwise" $ do
      1234567889 `shouldSatisfy` validate
      1234567887 `shouldNotSatisfy` validate
