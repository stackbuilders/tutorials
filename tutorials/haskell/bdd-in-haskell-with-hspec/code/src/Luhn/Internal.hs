module Luhn.Internal
  ( toDigits
  , fromDigits
  , doubleEveryOther
  , sumDigits
  )
  where

-- | Returns number digits as a list
toDigits :: Integer -> [Integer]
toDigits = go []
  where go xs n
          | n <= 0    = xs
          | otherwise = go (m:xs) d where (d,m) = divMod n 10

-- | Returns a number given its digits in a list
fromDigits :: [Integer] -> Integer
fromDigits = sum . zipWith (*) [10 ^ i | i <- [0..]] . reverse

-- | Double every other digit on the list starting from the right most number
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

-- | Sum all number in the list
-- | If a number is equal or greater than ten if sums its digits first
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)
