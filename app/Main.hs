{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}
module Main where

import Prelude
import Data.Foldable (fold)
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Web.Scotty qualified as Scotty
import Text.Blaze.Html.Renderer.Text qualified as Blaze
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA

main :: IO ()
main = Scotty.scotty 8080 $ do
  Scotty.middleware Wai.static
  Scotty.middleware Wai.logStdout

  Scotty.get "/" $ do
    Scotty.redirect "/public/index.html"

  Scotty.post "/user/:id" $ do
    id <- Scotty.pathParam "id"
    -- Scotty.liftIO $ print id
    Scotty.html $ do
      Blaze.renderHtml $ H.text $ "Hello, from POST " <> id

  Scotty.get "/user/:id" $ do
    id <- Scotty.pathParam "id"
    -- Scotty.liftIO $ print id
    Scotty.html $ do
      Blaze.renderHtml $ H.text $ "Hello, from GET " <> id
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
