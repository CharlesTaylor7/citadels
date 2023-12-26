{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}
module Main where

import Prelude
import Data.Foldable (fold)
import Web.Scotty qualified as Scotty
import Text.Blaze.Html qualified as Blaze 
import Text.Blaze.Html.Renderer.Text qualified as Blaze

main = Scotty.scotty 8080 $ do
    Scotty.get "/" $ do
      Scotty.html $ Blaze.renderHtml "Hello, World!"
