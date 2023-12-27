module Citadels.Pages.Lobby where

import Citadels.Prelude
import Citadels.Server.State 

import Data.HashMap.Strict qualified as HashMap

import Lucid 
import Lucid.Htmx
import Lucid.Extra
import Lucid.Base 


lobbyPage :: Player -> LobbyState -> Lucid.Html ()
lobbyPage player lobby =
    div_ [ class_ "flex flex-col gap-3 items-center justify-center" ] do

      h2_ [ class_ "mt-3 underline text-2xl font-semibold" ] do
        "Lobby"

      templateRegister player.username

      div_ [ class_ "p-7 rounded border border-slate-500" ] do

        h2_ [ class_ "underline text-2xl font-semibold" ] do
          "Players"

        ul_ [ id_ "players", class_ "list-disc" ] do
          templateLobbyPlayers lobby
       

templateLobbyPlayers :: LobbyState -> Html ()
templateLobbyPlayers lobby =  
    lobby.seatingOrder 
    & mapMaybe (\id -> lobby.players & HashMap.lookup id)
    & foldMap \player ->
        li_ [ id_ player.playerId.text, hxSwapOob_ "true"] do
          text_ player.username



templateRegister :: Text -> Lucid.Html ()
templateRegister username = 
  form_ 
    [ class_ "p-7 flex flex-col gap-3 items-center rounded border border-slate-500"
    , hxPost_ "/register"
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
