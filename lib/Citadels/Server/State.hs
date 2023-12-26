module Citadels.Server.State  where

import Relude hiding (id, get)

import Network.WebSockets qualified as WS

import Lucid.Base qualified as Lucid
import Data.HashTable (HashTable)
import Data.HashTable as Table
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie(..),defaultSetCookie)
import Data.Default
   
newtype SessionId = SessionId Text
  deriving newtype (Eq, Show, Hashable)


data Player = Player 
  { username :: Text
  , sessionId :: SessionId
  }

data GameState = GameState
  { players :: HashMap SessionId Player
  , seatingOrder :: List SessionId
  }
 
instance Default GameState where
  def = GameState 
    { players = mempty
    , seatingOrder = []
    }

type List a = [a]

data LobbyState = LobbyState 
  { players :: HashMap SessionId Player
  , seatingOrder :: List SessionId
  }

instance Default LobbyState where
  def = LobbyState 
    { players = mempty
    , seatingOrder = []
    }

{-# NOINLINE connections #-}
connections :: IORef (HashTable SessionId WS.Connection)
connections = unsafePerformIO do
  let initialSize = 10
  table <- Table.newWithDefaults initialSize
  newIORef table

{-# NOINLINE lobby #-}
lobby :: TVar LobbyState 
lobby = unsafePerformIO $
  newTVarIO def

{-# NOINLINE game #-}
game :: TVar LobbyState 
game = unsafePerformIO $
  newTVarIO def
