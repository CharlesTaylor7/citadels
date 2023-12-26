module Main where

import Citadels.Prelude

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS

import Citadels.Server.State as Global
import Citadels.Server.State (SessionId(..))

import Lucid qualified 
--import Optics
import Lucid.Htmx
import Lucid.Html5
import Web.Twain 
import Data.HashTable (HashTable)
import Data.HashTable as Table
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie(..),defaultSetCookie)
import Data.HashMap.Strict qualified  as HashMap
import Data.Maybe (fromJust)


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
  , register
  , websocket
  ]

scriptSrc_ :: Text -> Lucid.Html ()
scriptSrc_ src = script_ [ src_ src ] ("" :: ByteString)

text_ :: Text -> Lucid.Html ()
text_ = Lucid.toHtml


players :: LobbyState -> List Text
players lobby =  
  lobby.seatingOrder <&> \id ->
    let player = lobby.players & HashMap.lookup id & fromJust
    in player.username


-- | set session cookie here
index :: Middleware
index = get "/" $ do
  lobby <- readTVarIO Global.lobby
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
        div_ [ class_ "flex flex-col gap-3 items-center justify-center" ] do

          h2_ [ class_ "mt-3 underline text-2xl font-semibold" ] do
            "Lobby"

          form_ [ class_ "p-7 flex flex-col gap-3 items-center rounded border border-slate-500"
                , hxPost_ "/register"
                ] do
            div_ do
              label_ [ class_ "mr-3" ] "Username"

              -- | TODO: default value based on session
              input_ 
                [ class_ "px-3 border rounded border-slate-400 bg-slate-500"
                , type_ "text"
                , name_ "username" 
                ] 

            button_
              [ class_ "p-2 border rounded border-slate-400 bg-blue-500"
              , type_ "submit"
              ]
              "Submit"

          div_ [ class_ "p-7 rounded border border-slate-500" ] do

            h2_ [ class_ "underline text-2xl font-semibold" ] do
              "Players"

            ul_ [ class_ "list-disc" ] do
              players lobby & foldMap (li_ . text_)

register :: Middleware
register = post "/register" $ do
  putTextLn "Register"
  -- id <- SessionId <$> cookieParam "session"
  let id = SessionId "session"
  username <- param "username"
  req <- request

  atomically do
    lobby <- readTVar Global.lobby
    let player = lobby.players & HashMap.lookup id
    case player of
      Just p -> writeTVar Global.lobby $
        lobby 
          { players = lobby.players 
              & HashMap.insert id (p { username = username })
          }
      Nothing -> writeTVar Global.lobby $
        lobby 
            { players = lobby.players 
                & HashMap.insert id (Player { sessionId = id, username = username })
            , seatingOrder = lobby.seatingOrder <> [ id ]
            }


missing :: ResponderM a
missing = send $ html "Not found..."

websocket :: Middleware
websocket = get "/ws" $ do
  id <- cookieParam "session"
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
wsApp id pending = do
  let sessionId = SessionId id
  conn <- WS.acceptRequest pending
  connections <- readIORef Global.connections
  _ <- Table.insert connections sessionId conn
  putTextLn $ "WS connected, session: " <> show sessionId
  WS.withPingThread conn 30 (pure ()) $ do
    forever $ do
      msg  <- WS.receiveData conn 
      WS.sendTextData conn $ Lucid.renderBS "hello, " <> msg
      -- threadDelay 1_000_000
