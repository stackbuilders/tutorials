{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Asterius.Types
import           Control.Monad
import           Data.Aeson                 hiding (Object)
import qualified Data.Aeson                 as A
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Text

data User =
  User
    { name :: Text
    , age  :: Int
    }

$(deriveJSON defaultOptions 'User)

handleReq :: JSString -> JSString -> IO JSObject
handleReq method rawBody =
  case fromJSString method of
    "POST" ->
      let eitherUser :: Either String User
          eitherUser = eitherDecode (B8.pack $ fromJSString rawBody)
       in case eitherUser of
            Right _  -> js_new_response (toJSString "Success!") 200
            Left err -> js_new_response (toJSString err) 400
    _ -> js_new_response (toJSString "Not a valid method") 405

foreign export javascript "handleReq" handleReq :: JSString -> JSString -> IO JSObject

foreign import javascript "new Response($1, {\"status\": $2})"
  js_new_response :: JSString -> Int -> IO JSObject

main :: IO ()
main = putStrLn "CFW Cabal"
