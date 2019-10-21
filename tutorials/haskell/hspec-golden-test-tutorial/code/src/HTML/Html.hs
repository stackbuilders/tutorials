{-# LANGUAGE OverloadedStrings #-}


module HTML.Html  where

import           Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Html.Renderer.Pretty        (renderHtml)


htmlRendered :: Html -> String
htmlRendered page = renderHtml page

somePage :: Html
somePage = html $ do
    H.head $ do
        H.title "StackBuilders Tutorial."
    body $ do
       "Hello Golden Testers."

