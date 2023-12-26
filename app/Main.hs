module Main where

import Relude hiding (get)

import Data.Text qualified as T
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS
import Web.Twain -- qualified as Twain
import Network.Wai.Handler.WebSockets (websocketsApp)
import Network.WebSockets (defaultConnectionOptions)

main :: IO ()
main = do
  Warp.run 8080 $ 
    Wai.logStdout $
    Wai.static $ do
      twain

twain :: Wai.Application
twain = foldr ($)
  (notFound missing)
  [ get "/" index
  , get "/ws" websocket
  , get "echo/:name" echo
  ]

index :: ResponderM a
index = send $ redirect301 "/public/index.html"

echo :: ResponderM a
echo = do
  name <- param "name"
  send $ html $ "Hello, " <> name

missing :: ResponderM a
missing = send $ html "Not found..."

websocket :: ResponderM a 
websocket = do
  req <- request
  --let Just res = WaiWs.websocketsApp  WS.defaultConnectionOptions (websocketsApp) req

  send undefined

{-
mainWS :: IO ()
mainWS = do
  let port = 8080
  let settings = Warp.setPort port Warp.defaultSettings
  -- sapp <- scotty
  Warp.runSettings settings sapp
  -}



webSocketApp :: WS.ServerApp
webSocketApp pending = do
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

{-
scotty :: IO Wai.Application
scotty = Scotty.scottyApp $ do

{-
  Scott.get "/ws" $ do
    req <- Scotty.request
    WaiWs.websocketsApp WS.defaultConnectionOptions wsapp req
    -}

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
      --}
