module Citadels.Server.Main where

import Citadels.Prelude

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS
import Network.HTTP.Types.URI qualified as Url
import Network.Wai.Parse qualified as Wai

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
import Data.HashMap.Strict qualified as HashMap
import Web.Cookie (SetCookie(..), defaultSetCookie, sameSiteStrict) 
import Data.Maybe (fromJust)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import qualified Citadels.Server.State as Table
import qualified Network.Wai.Parse as Wai



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
  playerId <- Twain.cookieParamMaybe "cPlayerId" 
  playerId <- PlayerId <$> case playerId of
    Just id -> pure id
    Nothing -> UUID.toText <$> liftIO UUID.nextRandom

  username <- Twain.cookieParamMaybe "cUsername" 

  lobby <- readTVarIO Global.lobby
        
  Twain.send $ 
    Twain.withCookie' secureCookie 
      -- The leading c is to keep the cookie separate from other params
      { setCookieName = "cPlayerId"
      , setCookieValue = encodeUtf8 playerId.text 
      } $
    html do
      templatePage do 
        lobbyPage LobbyArgs 
          { lobby
          , playerId
          , username
          }



paramLookup :: Text -> [(Text, Text)] -> Maybe Text
paramLookup key pairs = snd <$> find ((== key) <<< fst) pairs

register :: Middleware
register = Twain.post "/register" $ do
  id <- PlayerId <$> Twain.cookieParam "cPlayerId"
  params  <- Twain.params
  putTextLn $ "Twain.params: " <> show params

  let username = fromJust $ paramLookup "username" params

  readTVarIO Global.lobby >>= \lobby -> putTextLn $ "before: " <> show lobby
  lobby <- atomically do
    lobby <- readTVar Global.lobby
    let player = lobby.players & HashMap.lookup id
    let 
      updatedLobby :: LobbyState
      updatedLobby = case player of
        Just p -> lobby 
            { players = lobby.players 
                & HashMap.insert id (p { username = username } :: Player)
            }
        Nothing -> lobby 
              { players = lobby.players 
                  & HashMap.insert id (Player { playerId = id, username = username })
              , seatingOrder = lobby.seatingOrder <> [ id ]
              }
    writeTVar Global.lobby updatedLobby
    pure updatedLobby

  putTextLn $ "after: " <> show lobby

  broadcast do
    templateLobbyPlayers lobby

  Twain.send $ 
    Twain.withCookie' secureCookie
      { setCookieName = "cUsername"
      , setCookieValue = encodeUtf8 username
      } $
    Twain.text ""

html :: Lucid.Html Unit -> Response
html = Twain.html <<< Lucid.renderBS

missing :: ResponderM a
missing = do
  Twain.send $ Twain.text "404"

websocket :: Middleware
websocket = Twain.get "/ws" $ do
  id <- PlayerId <$> Twain.cookieParam "cPlayerId"
  req <- Twain.request
  let app = wsApp id
  case Wai.websocketsApp WS.defaultConnectionOptions app req of 
    Just res -> Twain.send res
    Nothing -> missing


-- | TODO: prod only
secureCookie :: SetCookie
secureCookie = defaultSetCookie 
  { 
  --setCookieSecure = True,
   -- setCookieHttpOnly = True 
   setCookieSameSite = Just sameSiteStrict
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

{-
 - Doesn't work
formBodyParams :: ResponderM [(Text, Text)]
formBodyParams = do
  request <- Twain.request
  (params, files) <- liftIO $
    Wai.parseRequestBody
      --Wai.defaultParseRequestBodyOptions 
      Wai.lbsBackEnd 
      request

  pure $ params <&> bimap decodeUtf8 decodeUtf8
  -}
