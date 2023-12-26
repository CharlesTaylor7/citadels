module Main where

import Relude hiding (get)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS

import Web.Twain 

main :: IO ()
main = 
  Warp.runSettings settings $ 
    middleware twain

  where
    middleware = Wai.logStdout . Wai.static
    
    settings = 
      Warp.defaultSettings
      & Warp.setPort 8080 
      & Warp.setOnExceptionResponse Warp.exceptionResponseForDebug 

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
  case Wai.websocketsApp  WS.defaultConnectionOptions wsApp req of 
    Just res -> send res
    Nothing -> missing


wsApp :: WS.ServerApp
wsApp pending = do
  putText "ws connected"
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) $ do
    (msg :: Text) <- WS.receiveData conn
    WS.sendTextData conn $ ("initial> " :: Text) <> msg
