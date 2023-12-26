module Main where

import Relude hiding (get)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS

import Lucid.Base qualified as Lucid
import Lucid.Html5
import Web.Twain 

main :: IO ()
main = do
  let
    port = 8080
    settings = 
      Warp.defaultSettings
      & Warp.setPort port 
      & Warp.setOnExceptionResponse Warp.exceptionResponseForDebug 
  putTextLn $ "Listening on port " <> show port 
  Warp.runSettings settings $ 
    middleware twain

  where
    middleware = Wai.logStdoutDev . Wai.static
    

twain :: Wai.Application
twain = foldr ($)
  (notFound missing)
  [ get "/" index
  , get "echo/:name" echo
  , get "/ws" websocket
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
  case Wai.websocketsApp  WS.defaultConnectionOptions wsApp req of 
    Just res -> send res
    Nothing -> missing


wsApp :: WS.ServerApp
wsApp pending = do
  conn <- WS.acceptRequest pending
  putTextLn "WS connected"
  WS.withPingThread conn 30 (pure ()) $ do
    forever $ do
      msg  <- WS.receiveData conn 
      WS.sendTextData conn $ Lucid.renderBS "hello, " <> msg
      -- threadDelay 1_000_000
