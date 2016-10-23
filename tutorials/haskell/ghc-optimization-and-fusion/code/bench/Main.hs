module Main (main) where

-- import Control.DeepSeq
import Criterion.Main
import Goaf

main :: IO ()
main = defaultMain
  [ bench "inlining0" (nf inlining0 0)
  , bench "inlining1" (nf inlining1 0) ]
