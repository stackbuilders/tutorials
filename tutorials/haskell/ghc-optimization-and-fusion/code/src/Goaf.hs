module Goaf
  ( inlining0
  , inlining1 )
where

inlining0 :: Int -> Int
inlining0 x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]

inlining1 :: Int -> Int
inlining1 x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
{-# INLINEABLE inlining1 #-}
