module Citadels.Server.State  where

import Citadels.Prelude

import Network.WebSockets qualified as WS

import Lucid.Base qualified as Lucid
import Data.HashTable (HashTable)
import Data.Vector (Vector)
import Data.HashTable as Table
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie(..),defaultSetCookie)
import Data.Default
   
newtype PlayerId = PlayerId { text :: Text }


data Player = Player 
  { playerId :: PlayerId
  , username :: Text
  }
  -- deriving stock (Generic)

data LobbyState = LobbyState 
  { players :: HashMap PlayerId Player
  , seatingOrder :: List PlayerId
  }
  deriving stock (Show)
   }

{-# NOINLINE connections #-}
connections :: IORef (HashTable PlayerId WS.Connection)
connections = unsafePerformIO do
  let initialSize = 10
  table <- Table.newWithDefaults initialSize
  newIORef table

{-# NOINLINE lobby #-}
lobby :: TVar LobbyState 
lobby = unsafePerformIO $
  newTVarIO def
