module Citadels.Prelude 
  ( module Relude
  , unit
  , Unit
  , List
  ) where

import Relude hiding ((.), get, id)

type List a = [a]
type Unit = ()

unit :: ()
unit = ()
