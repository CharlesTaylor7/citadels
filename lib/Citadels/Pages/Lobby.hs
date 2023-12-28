module Citadels.Pages.Lobby where

import Citadels.Prelude
import Citadels.Server.State 

import Data.HashMap.Strict qualified as HashMap

import Lucid 
import Lucid.Htmx
import Lucid.Extra
import Lucid.Base 


data LobbyArgs = LobbyArgs
  { lobby :: LobbyState
  , playerId :: PlayerId
  , username :: Maybe Text
  }

lobbyPage :: LobbyArgs -> Lucid.Html ()
lobbyPage args@(LobbyArgs { playerId, lobby })=
  main_ [ wsConnect_ "/ws", class_ "bg-slate-700 h-full text-slate-200 text-xl"] do
    div_ [ class_ "flex flex-col gap-3 items-center justify-center" ] do
      h2_ [ class_ "mt-3 underline text-2xl font-semibold" ] do
        "Lobby"

      templateRegister username

      div_ [ class_ "p-7 rounded border border-slate-500" ] do
        h2_ [ class_ "underline text-2xl font-semibold" ] do
          "Players"

        templateLobbyPlayers lobby

    where
      username = fromMaybe "" do
        (.username) <$> HashMap.lookup playerId lobby.players <|> args.username
       

templateLobbyPlayers :: LobbyState -> Html ()
templateLobbyPlayers lobby = do
  ul_ [ id_ "players", class_ "list-disc", hxSwapOob_ "true" ] do
    lobby.seatingOrder 
      & mapMaybe (\id -> lobby.players & HashMap.lookup id)
      & foldMap \player ->
          li_ [ id_ player.playerId.text] do
            text_ player.username



templateRegister :: Text -> Lucid.Html ()
templateRegister username = 
  form_ 
    [ class_ "p-7 flex flex-col gap-3 items-center rounded border border-slate-500"
    , hxPost_ "/register"
    , hxTarget_ "#players"
    ] do
    div_ do
      label_ [ class_ "mr-3" ] "Username"

      input_ 
        [ class_ "px-3 border rounded border-slate-400 bg-slate-500"
        , type_ "text"
        , name_ "username" 
        , value_ username 
        ] 

    button_
      [ class_ "p-2 border rounded border-slate-400 bg-blue-500"
      , type_ "submit"
      ] do
      "Set name"
