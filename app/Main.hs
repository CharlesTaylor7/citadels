module Main where

import Relude hiding (id, get)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS

import Lucid.Base qualified as Lucid
import Lucid.Html5
import Web.Twain 
import Data.HashTable (HashTable)
import Data.HashTable as Table
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie(..),defaultSetCookie)

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
  [ index
  , echo
  , websocket
  ]

index :: Middleware
index = get "/" $ do
  send $ redirect301 "/public/index.html"

echo :: Middleware 
echo = get "/echo/:name" $ do
  name <- pathParam "name"
  send $ html $ "Hello, " <> name

missing :: ResponderM a
missing = send $ html "Not found..."

websocket :: Middleware
websocket = get "/ws/:id" $ do
  id <- pathParam "id"
  req <- request
  let app = wsApp id
  case Wai.websocketsApp WS.defaultConnectionOptions app req of 
    Just res -> send res
    Nothing -> missing


setCookie :: Request -> SetCookie
setCookie req = defaultSetCookie 
  { setCookieSecure = isSecure req
  , setCookieHttpOnly = True 
  }


wsApp :: Text -> WS.ServerApp
wsApp sessionId pending = do
  conn <- WS.acceptRequest pending
  conns <- readIORef connections
  _ <- Table.insert conns sessionId conn
  putTextLn $ "WS connected, session: " <> sessionId
  WS.withPingThread conn 30 (pure ()) $ do
    forever $ do
      msg  <- WS.receiveData conn 
      WS.sendTextData conn $ Lucid.renderBS "hello, " <> msg
      -- threadDelay 1_000_000
