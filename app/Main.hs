{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}
module Main where

import Prelude
import Data.Foldable (fold)
import Web.Scotty qualified as Scotty
import Text.Blaze.Html.Renderer.Text qualified as Blaze
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Network.Wai.Middleware.Static qualified as Wai
import System.IO (stdout, hSetBuffering, BufferMode (LineBuffering))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  Scotty.scotty 8080 $ do
    Scotty.middleware Wai.static
    Scotty.get "/" $ do
      Scotty.liftIO $ putStrLn "root"
      Scotty.redirect "/public/index.html"

    Scotty.get "/user/:id" $ do
      Scotty.liftIO $ putStrLn "/user"
      id <- Scotty.pathParam "id"
      Scotty.liftIO $ print id
      Scotty.html $ do
        Blaze.renderHtml $ H.text $ "Hello, " <> id
      {-
      <form class="mt-10" hx-put="/user/1" hx-target="this" hx-swap="outerHTML">
      -swap="outerHTML">
        <label>Username</label>
        <input class="border rounded border-slate-400 bg-slate-600 px-3" type="text" name="username" value="Joe">
        <button class="p-2 m-3 rounded bg-green-600">Submit</button>
        <button class="p-2 m-3 rounded bg-red-400" hx-get="/user/1">Cancel</button>
      </form>
    </main>

-}
      --Scotty.html $ Blaze.renderHtml "Hello, World!"
