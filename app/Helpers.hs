module Helpers (module Helpers) where
import Control.Monad.State (State, evalState, get, put)
import System.Random (Random, StdGen, mkStdGen, random)
-- see:
-- https://stackoverflow.com/questions/2110535/sampling-sequences-of-random-numbers-in-haskell

-- alias
type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: Random a => R a
rand = do
  gen <- get
  let (r,gen') = random gen
  put gen'
  return r
