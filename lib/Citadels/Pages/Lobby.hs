module Citadels.Pages.Lobby where

import Citadels.Prelude
import Citadels.Server.State 

import Data.HashMap.Strict qualified as HashMap

import Lucid qualified 
import Lucid.Htmx
import Lucid.Html5
import Lucid.Extra


lobbyPage :: LobbyState -> Lucid.Html ()
lobbyPage lobby =
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

    where
    players :: LobbyState -> List Text
    players lobby =  
      lobby.seatingOrder & mapMaybe \id ->
        lobby.players & HashMap.lookup id <&> (.username)


templateRegister :: Text -> Lucid.Html ()
templateRegister username = 
  form_ 
    [ class_ "p-7 flex flex-col gap-3 items-center rounded border border-slate-500"
    , hxPost_ "/register"
    , hxSwap_ "outerHTML"
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
