
module Test.Main where

import Prelude

import Console (log)
import Control.Monad.Eff

foreign import throwError
  """
  function throwError(msg) {
    throw new Error(msg)
  }
  """ :: forall e. String -> Eff e Unit

check :: forall e a. (Show a, Ord a) => Ordering -> a -> a -> Eff e Unit
check expected a b =
  let actual = compare a b
  in if actual == expected
    then return unit
    else throwError (show a <> " `compare` " <> show b <> ": " <>
                      "expected " <> show expected <> ", got " <> show actual)

main = do
  check EQ [] ([] :: Array Number)
  check LT [] [0]
  check GT [0] []
  check EQ [1,2,3] [1,2,3]
  check LT [1,1,3] [1,2,3]
  check GT [1,3,3] [1,2,3]

  log "All good!"
