module Luhn
  ( validate
  )
  where

import Luhn.Internal

-- | Validates a number using the Luhn algorithm
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
