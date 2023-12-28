module Citadels.Pages.Game where

import Citadels.Prelude
import Citadels.Server.State 

import Data.HashMap.Strict qualified as HashMap

import Lucid 
import Lucid.Htmx
import Lucid.Extra
import Lucid.Base 



gamePage :: GameState -> PlayerId -> Lucid.Html ()
gamePage game playerId =
  main_ [ wsConnect_ "/ws", class_ "bg-slate-700 h-full text-slate-200 text-xl"] do
    div_ [ class_ "flex flex-col gap-3 items-center justify-center" ] do
      h2_ [ class_ "mt-3 underline text-2xl font-semibold" ] do
        "Game"

      div_ [ class_ "p-7 rounded border border-slate-500" ] do
        h2_ [ class_ "underline text-2xl font-semibold" ] do
          "Players"

        templateGamePlayers game

       

templateGamePlayers :: GameState -> Html ()
templateGamePlayers game = do
  ul_ [ id_ "players", class_ "list-disc", hxSwapOob_ "true" ] do
    game.seatingOrder 
      & mapMaybe (\id -> game.players & HashMap.lookup id)
      & foldMap \player ->
          li_ [ id_ player.playerId.text] do
            text_ player.username
