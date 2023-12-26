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
import Citadels.Pages.Lobby 

import Lucid qualified 
--import Optics
import Lucid.Htmx
import Lucid.Html5
import Lucid.Extra
import Web.Twain 
import Data.HashTable (HashTable)
import Data.HashTable qualified as Table
import Web.Cookie (SetCookie(..), defaultSetCookie, sameSiteStrict) 
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import Citadels.Templates (templateHead)


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
    middleware = Wai.logStdoutDev <<< Wai.static
    

twain :: Wai.Application
twain = foldr ($)
  (notFound missing)
  [ index
  , register
  , websocket
  ]

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
      templateHead
      body_ [ class_ "bg-slate-700 h-full text-slate-200 text-xl"] do
        div_ [ class_ "flex flex-col gap-3 items-center justify-center" ] do

          h2_ [ class_ "mt-3 underline text-2xl font-semibold" ] do
            "Lobby"

          templateRegister ""         

          div_ [ class_ "p-7 rounded border border-slate-500" ] do

            h2_ [ class_ "underline text-2xl font-semibold" ] do
              "Players"

            ul_ [ id_ "players", class_ "list-disc" ] do
              players lobby & foldMap (li_ <<< text_)

register :: Middleware
register = post "/register" $ do
  id <- SessionId <$> cookieParam "session"
  username <- param "username"

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

  send $ html $ Lucid.renderBS $ do
    templateRegister username
    div_ [ id_ "players", hxSwapOob_ "beforeend" ] do
      li_ do
        text_ username


missing :: ResponderM a
missing = do
  send $ html "Not found..."

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
  , setCookieSameSite = Just sameSiteStrict
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
