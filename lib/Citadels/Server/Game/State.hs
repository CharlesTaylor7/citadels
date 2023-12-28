module Citadels.Server.Game.State  where

import Citadels.Prelude

import Network.WebSockets qualified as WS

import Lucid.Base qualified as Lucid
import Data.HashTable (HashTable)
import Data.Vector (Vector)
import Data.HashTable as Table
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie(..),defaultSetCookie)
import Data.Default
import Citadels.Types
   
newtype PlayerId = PlayerId { text :: Text }
  deriving newtype (Eq, Hashable)
  deriving stock (Show)


data Player = Player 
  { playerId :: PlayerId
  , username :: Text
  , gold :: Int
  , hand :: Seq District
  , city :: Seq District
  , roles :: Seq Character
  }
  --deriving stock (Show)
  -- deriving stock (Generic)

data GameState = GameState
  { players :: HashMap PlayerId Player
  , seatingOrder :: Vector PlayerId
  , characters :: Vector Character
  , crowned :: PlayerId
  }
  -- deriving stock (Generic)
 
instance Default GameState where
  def = GameState 
    { players = mempty
    , seatingOrder = fromList []
    , characters = fromList []
    , crowned = PlayerId ""
    }

{-# NOINLINE game #-}
game :: TVar GameState 
game = unsafePerformIO $
  newTVarIO def
