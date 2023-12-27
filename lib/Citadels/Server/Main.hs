module Citadels.Server.Main where

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
import Citadels.Templates

import Lucid (Html)
import Lucid qualified
import Web.Twain (Middleware, Response, ResponderM)
import Web.Twain qualified as Twain
import Data.HashTable (HashTable)
import Data.HashTable qualified as Table
import Web.Cookie (SetCookie(..), defaultSetCookie, sameSiteStrict) 
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromJust)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
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
  (Twain.notFound missing)
  [ index
  , register
  , websocket
  ]

tableLookup :: Eq k => MonadIO m => IORef (HashTable k v) -> k -> m (Maybe v)
tableLookup ref key = liftIO do
  table <- readIORef ref 
  Table.lookup table key

-- | set session cookie here
index :: Middleware
index = Twain.get "/" $ do
  maybeId :: _ Text <- Twain.cookieParamMaybe "playerId" 
  playerId <- PlayerId <$> case maybeId of
    Just id -> pure id
    Nothing -> liftIO $ UUID.toText <$> UUID.nextRandom

  lobby <- atomically do
    lobby <- readTVar Global.lobby

    let username = lobby.players & HashMap.lookup playerId & maybe "" (.username)
    let player = Player { playerId, username }
    let updatedLobby :: LobbyState 
        updatedLobby = lobby { players = lobby.players & HashMap.insert playerId player }

    writeTVar Global.lobby updatedLobby
    pure updatedLobby
    
  let cookie = secureCookie { setCookieName = "playerId", setCookieValue = encodeUtf8 playerId.text }
  Twain.send $ Twain.withCookie' cookie $ html do
    templatePage do lobbyPage playerId lobby

sendWithCookie :: Maybe SetCookie -> Response -> ResponderM a
sendWithCookie (Just cookie) = Twain.send <<< Twain.withCookie' cookie
sendWithCookie Nothing = Twain.send 


{-
newSessionCookie :: MonadIO m => m SetCookie
newSessionCookie = liftIO do
  playerId <- UUID.nextRandom
  playerId <- UUID.nextRandom

  _ <- Table.insert ids (PlayerId $ show playerId ) (PlayerId $ show playerId)
    
  pure $ secureCookie
    { setCookieName = "playerId"
    , setCookieValue = show playerId
    }
-}


register :: Middleware
register = Twain.post "/register" $ do
  username <- Twain.param "username"
  id <- PlayerId <$> Twain.cookieParam "playerId"

  putTextLn $ "playerId: " <> id.text

  lobby <- atomically do
    lobby <- readTVar Global.lobby
    let player = lobby.players & HashMap.lookup id
    let 
      updatedLobby :: LobbyState
      updatedLobby = case player of
        Just p -> lobby 
            { players = lobby.players 
                & HashMap.insert id (p { username = username })
            }
        Nothing -> lobby 
              { players = lobby.players 
                  & HashMap.insert id (Player { playerId = id, username = username })
              , seatingOrder = lobby.seatingOrder <> [ id ]
              }
    writeTVar Global.lobby updatedLobby
    pure updatedLobby

  putTextLn $ show lobby

  broadcast do
    templateLobbyPlayers lobby

  Twain.send $ Twain.text ""

html :: Lucid.Html Unit -> Response
html = Twain.html <<< Lucid.renderBS

missing :: ResponderM a
missing = do
  Twain.send $ html "Not found..."

websocket :: Middleware
websocket = Twain.get "/ws" $ do
  id <- PlayerId <$> Twain.cookieParam "playerId"
  req <- Twain.request
  let app = wsApp id
  case Wai.websocketsApp WS.defaultConnectionOptions app req of 
    Just res -> Twain.send res
    Nothing -> missing


secureCookie :: SetCookie
secureCookie = defaultSetCookie 
  { setCookieSecure = True
  , setCookieHttpOnly = True 
  , setCookieSameSite = Just sameSiteStrict
  }

broadcast :: MonadIO m => Html () -> m ()
broadcast html = liftIO do
  putTextLn "begin broadcast"
  conns <- readIORef Global.connections
  let bytes = Lucid.renderBS html
  pairs <- atomically $ Table.readAssocs conns

  putTextLn $ "connection count: " <> show (length pairs)
  for_ pairs $ \(_, conn) ->
    WS.sendTextData conn bytes

  putTextLn "end broadcast"


wsApp :: PlayerId -> WS.ServerApp
wsApp playerId pending = do
  conn <- WS.acceptRequest pending
  connections <- readIORef Global.connections
  _ <- Table.insert connections playerId conn
  putTextLn $ "WS connected, session: " <> playerId.text

  lobby <- readTVarIO Global.lobby
  WS.sendTextData conn $ Lucid.renderBS $
    templateLobbyPlayers lobby 

  WS.withPingThread conn 30 (pure ()) $ do
    forever $ do
      msg  <- WS.receiveData conn 
      WS.sendTextData conn $ Lucid.renderBS "hello, " <> msg
