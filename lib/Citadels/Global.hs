module Citadels.Global 
  ( connections
  ) where

import Relude hiding (id, get)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.Wai.Middleware.Static qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.WebSockets qualified as WS

import Lucid.Base qualified as Lucid
import Lucid.Html5
import Web.Twain 
import Data.HashTable (HashTable)
import Data.HashTable as Table
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie(..),defaultSetCookie)


{-# NOINLINE connections #-}
connections :: IORef (HashTable Text WS.Connection)
connections = unsafePerformIO $ do
  let initialSize = 10
  table <- Table.newWithDefaults initialSize
  newIORef table


