-- | Guiding principle is this should only export items from vector, containers, relude or base. 
-- I want to avoid putting custom functions in here.
-- But aliases are fine.
module Citadels.Prelude 
  ( module Relude
  -- | Simple Aliases
  , unit
  , Unit
  , List
  -- | Re export from vector
  , Vector
  -- | Re exports from `base`
  , for
  , try
  ) where

import Relude hiding ((.), get, id)
import Data.Traversable (for)
import Control.Exception (try)

import Data.Vector (Vector)

type List a = [a]
type Unit = ()

unit :: Unit
unit = ()
