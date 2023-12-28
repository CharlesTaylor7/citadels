module Citadels.Types where

import Relude

data District = District
  { name :: Text
  , cost :: Int 
  , suit :: Suit
  , set :: CardSet
  , count :: Int
  }

data Character = Character
  { name :: Text
  , rank :: Int
  , set :: CardSet
  , description :: Text
  }

data Suit = Red | Green | Blue | Yellow | Purple 

data CardSet = Base | DarkCity | Citadels2016
