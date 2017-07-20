module Main where

import Types (Scientist)
import Language.PureScript.Bridge (writePSTypes, buildBridge, defaultBridge, mkSumType)
import Data.Proxy (Proxy(..))

main :: IO ()
main = writePSTypes "../frontend/src" (buildBridge defaultBridge) myTypes
  where
    myTypes = [ mkSumType (Proxy :: Proxy Scientist)
              ]
