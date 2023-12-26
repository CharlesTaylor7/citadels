module Citadels.Pages.Lobby where


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
