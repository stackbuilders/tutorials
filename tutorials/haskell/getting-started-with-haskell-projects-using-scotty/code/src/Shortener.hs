{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types (status404)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty

shortener :: IO ()
shortener = do
  urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsR
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"
            H.table $
              for_ (M.toList urls) $ \(i, url) ->
                H.tr $ do
                  H.td (H.toHtml i)
                  H.td (H.text url)
    post "/" $ do
      url <- param "url"
      liftIO $ modifyIORef urlsR $
        \(i, urls) ->
          (i + 1, M.insert i url urls)
      redirect "/"
    get "/:n" $ do
      n <- param "n"
      (_, urls) <- liftIO $ readIORef urlsR
      case M.lookup n urls of
        Just url ->
          redirect (LT.fromStrict url)
        Nothing ->
          raiseStatus status404 "not found"
