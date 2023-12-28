module Citadels.Shuffle 
  ( fisherYates
  ) where

import Citadels.Prelude

import System.Random qualified as Random

import Control.Monad.ST (runST)

import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV


type StateAlias s a = s -> (a, s)


-- | shuffle a vector randomly
fisherYates :: Vector a -> State Random.StdGen (Vector a)
fisherYates vector = state $ fisherYates_ vector


fisherYates_ :: Vector a -> StateAlias Random.StdGen (Vector a)
fisherYates_ vector g = runST $ do
  mutable <- V.thaw vector
  let n = V.length vector
  let (swaps, g') = runState (fisherYatesSwaps n) g
  for_ swaps $ \(i, j) -> MV.swap mutable i j
  randomized <- V.unsafeFreeze mutable
  pure (randomized, g')


-- | a random sequence of indices used by the swaps of the yates shuffle
fisherYatesSwaps :: Int -> State Random.StdGen [(Int, Int)]
fisherYatesSwaps n = do
  for [(n-1),(n-2)..0] $ \i -> do
    j <- state $ Random.uniformR (0, i)
    pure (i, j)
