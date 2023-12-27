module Main where

import Citadels.Prelude

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS

import Citadels.Server.State qualified as Global
import Citadels.Server.State 
import Citadels.Pages.Lobby 

import Lucid 
import Lucid.Htmx
import Lucid.Extra
import Web.Twain 
import Data.HashTable (HashTable)
import Data.HashTable qualified as Table
import Web.Cookie (SetCookie(..), defaultSetCookie, sameSiteStrict) 
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Citadels.Templates (templateHead)
import qualified Citadels.Server.State as Table


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

-- | set session cookie here
index :: Middleware
index = get "/" $ do
  playerId :: _ Text <- cookieParamMaybe "sessionId"
  lobby <- readTVarIO Global.lobby

  let 
    response = 
        html $ Lucid.renderBS do
        doctypehtml_ do
          templateHead
          lobbyPage lobby

  case playerId of
    Just _ -> send response
    Nothing -> do
      cookie <- newSessionCookie
      send $ withCookie' cookie response


sendWithCookie :: Maybe SetCookie -> Response -> ResponderM a
sendWithCookie (Just cookie) = send <<< withCookie' cookie
sendWithCookie Nothing = send 


newSessionCookie :: MonadIO m => m SetCookie
newSessionCookie = liftIO do
  sessionId <- UUID.nextRandom
  playerId <- UUID.nextRandom

  ids <- readIORef Global.playerIds
  _ <- Table.insert ids (SessionId $ show sessionId ) (PlayerId $ show playerId)
    
  pure $ secureCookie
    { setCookieName = "sessionId"
    , setCookieValue = show sessionId
    }


register :: Middleware
register = post "/register" $ do
  username <- param "username"
  sessionId <- SessionId <$> cookieParam "sessionId"

  putTextLn $ "session: " <> sessionId.text
  playerIds <- readIORef Global.playerIds
  id <- liftIO do
    Just id <- Table.lookup playerIds sessionId 
    pure id

  putTextLn $ "player: " <> id.text

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
                & HashMap.insert id (Player { playerId = id, username = username })
            , seatingOrder = lobby.seatingOrder <> [ id ]
            }

  send $ html $ Lucid.renderBS $ do
    templateRegister username
    div_ [ id_ "players", hxSwapOob_ "beforeend" ] do
      li_ [ id_ id.text, hxSwapOob_ "true"] do
        text_ username


missing :: ResponderM a
missing = do
  send $ html "Not found..."

websocket :: Middleware
websocket = get "/ws" $ do
  id <- SessionId <$> cookieParam "sessionId"
  req <- request
  let app = wsApp id
  case Wai.websocketsApp WS.defaultConnectionOptions app req of 
    Just res -> send res
    Nothing -> missing


secureCookie :: SetCookie
secureCookie = defaultSetCookie 
  { setCookieSecure = True
  , setCookieHttpOnly = True 
  , setCookieSameSite = Just sameSiteStrict
  }


wsApp :: SessionId -> WS.ServerApp
wsApp sessionId pending = do
  conn <- WS.acceptRequest pending
  connections <- readIORef Global.connections
  _ <- Table.insert connections sessionId conn
  putTextLn $ "WS connected, session: " <> sessionId.text

  lobby <- readTVarIO Global.lobby
  WS.sendTextData conn $ Lucid.renderBS $
    templateLobbyPlayers lobby 

  WS.withPingThread conn 30 (pure ()) $ do
    forever $ do
      msg  <- WS.receiveData conn 
      WS.sendTextData conn $ Lucid.renderBS "hello, " <> msg
