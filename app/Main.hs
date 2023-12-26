{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}
module Main where

import Prelude
import Data.Foldable (fold)
import Web.Scotty qualified as Scotty
import Text.Blaze.Html qualified as Blaze 
import Text.Blaze.Html.Renderer.Text qualified as Blaze
import Network.Wai.Middleware.Static qualified as Wai

main :: IO ()
main = Scotty.scotty 8080 $ do
    Scotty.middleware Wai.static
    Scotty.get "/" $ do
      Scotty.redirect "/public/index.html"
      --Scotty.html $ Blaze.renderHtml "Hello, World!"
