{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Web.Scotty

main = scotty 8080 $ do
    get "/" $ do
      html "Hello, World!"

    get "/:word" $ do
        beam <- pathParam "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

