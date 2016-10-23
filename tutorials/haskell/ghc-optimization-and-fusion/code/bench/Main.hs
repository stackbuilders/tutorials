module Main (main) where

import Criterion.Main
import Goaf

main :: IO ()
main = defaultMain
  [ bench "inlining0"      (nf inlining0 0)
  , bench "inlining1"      (nf inlining1 0)
  -- , bench "special0"     (nf special0  0)
  , bench "special0_alt"   (nf special0_alt 0)
  , bench "nofusion0"      (nf nofusion0 [0..1000000])
  , bench "manuallyFused"  (nf manuallyFused [0..1000000])
  , bench "manuallyFused'" (nf manuallyFused' [0..1000000])
  , bench "fuseda"         (nf fuseda 1000000)
  , bench "fused1"         (nf fused1 [0..1000000]) ]

special0_alt :: Int -> Int
special0_alt x = special0' x `rem` 10
{-# SPECIALIZE INLINE special0' :: Int -> Int #-}
