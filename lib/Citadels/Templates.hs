module Citadels.Templates where

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
import Lucid.Extra
import Web.Twain 
import Data.HashTable (HashTable)
import Data.HashTable as Table
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie(..), defaultSetCookie)
import Data.HashMap.Strict qualified  as HashMap
import Data.Maybe (fromJust)


templateHead :: Lucid.Html ()
templateHead = 
  head_ do
    title_ "Citadels"
    meta_ [charset_ "utf-8"]
    link_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    link_ [rel_ "shortcut icon", href_ "/public/favicon.ico"]
    link_ [rel_ "stylesheet", href_ "/public/index.css"]
    scriptSrc_ "https://unpkg.com/htmx.org@1.9.10"
    scriptSrc_ "https://unpkg.com/htmx.org@1.9.10/dist/ext/ws.js" 
    scriptSrc_ "https://unpkg.com/hyperscript.org@0.9.12"

