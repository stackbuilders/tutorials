module Factorial (fact) where

fact :: Int -> Int
fact n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (n*acc)

foreign export javascript "fact" fact :: Int -> Int
