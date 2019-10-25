module FIZBUZ.FizBuz where 

fizzBuzz :: [Int] -> [String]
fizzBuzz list = map fizzOrBuzz list

fizzOrBuzz ::Int -> String
fizzOrBuzz n | n `mod` 15 == 0  = "FizzBuzz"
             | n `mod` 3  == 0  = "Fizz"
             | n `mod` 5  == 0  = "Buzz"
             | otherwise        = show n
