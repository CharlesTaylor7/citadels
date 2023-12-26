module Citadels.Types where

import Relude

data Card = Card
  { name :: Text
  , cost :: Int 
  , suit :: Suit
  }

data Suit = Red | Green | Blue | Yellow | Purple 
