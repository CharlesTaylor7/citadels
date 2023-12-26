module Main where

import Relude hiding (id, get)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS

import Citadels.Server.State as Global
import Citadels.Server.State (SessionId(..))

import Lucid qualified 
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

none :: ByteString
none = ""

scriptSrc_ :: Text -> Lucid.Html ()
scriptSrc_ src = script_ [ src_ src ] ("" :: ByteString)

text_ :: Text -> Lucid.Html ()
text_ = Lucid.toHtml


players :: [Text]
players = [ "alice", "bob", "carl"]

-- | set session cookie here
index :: Middleware
index = get "/" $ do
  send $ html $ Lucid.renderBS do
    doctypehtml_ do
      head_ do
        title_ "Citadels"
        meta_ [charset_ "utf-8"]
        link_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        link_ [rel_ "shortcut icon", href_ "/public/favicon.ico"]
        link_ [rel_ "stylesheet", href_ "/public/index.css"]
        scriptSrc_ "https://unpkg.com/htmx.org@1.9.10"
        scriptSrc_ "https://unpkg.com/htmx.org@1.9.10/dist/ext/ws.js" 
        scriptSrc_ "https://unpkg.com/hyperscript.org@0.9.12"
      body_ [ class_ "bg-slate-700 h-full text-slate-200 text-xl"] do
        div_ [ class_ "flex flex-col items-center justify-center" ] do
          div_ [ class_ "mt-10 p-10 rounded border border-slate-500" ] do
            h2_ [ class_ "underline text-2xl font-semibold" ] 
              "players"

            ul_ [ class_ "list-disc" ] do
              players & foldMap (li_ . text_)

echo :: Middleware 
echo = get "/echo/:name" $ do
  name <- pathParam "name"
  send $ html $ "Hello, " <> name

missing :: ResponderM a
missing = send $ html "Not found..."

websocket :: Middleware
websocket = get "/ws" $ do
  id <- cookieParam "id"
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
  connections <- readIORef Global.connections
  _ <- Table.insert connections (SessionId sessionId) conn
  putTextLn $ "WS connected, session: " <> sessionId
  WS.withPingThread conn 30 (pure ()) $ do
    forever $ do
      msg  <- WS.receiveData conn 
      WS.sendTextData conn $ Lucid.renderBS "hello, " <> msg
      -- threadDelay 1_000_000
