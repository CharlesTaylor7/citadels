{-# LANGUAGE OverloadedStrings, ImportQualifiedPost, ScopedTypeVariables #-}
module Main where

import Relude

import Data.Text qualified as T
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS
import Text.Blaze.Html.Renderer.Text qualified as Blaze
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Web.Scotty qualified as Scotty
import Control.Concurrent (threadDelay)


main :: IO ()
main = do
  let port = 8080
  let settings = Warp.setPort port Warp.defaultSettings
  sapp <- scotty
  Warp.runSettings settings sapp



wsapp :: WS.ServerApp
wsapp pending = do
  putText "ws connected"
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    (msg :: Text) <- WS.receiveData conn
    WS.sendTextData conn $ ("initial> " :: Text) <> msg
    {-
    forever $ do
      WS.sendTextData conn ("loop data" :: Text)
      threadDelay $ 1 * 1000000
      -}


scotty :: IO Wai.Application
scotty = Scotty.scottyApp $ do
  Scotty.middleware Wai.static
  Scotty.middleware Wai.logStdout

  Scott.get "/ws" $ do
    req <- Scotty.request
    WaiWs.websocketsApp WS.defaultConnectionOptions wsapp req

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
