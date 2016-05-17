module Test.Main where

import Prelude

type AlmostEff = Unit -> Unit

main :: AlmostEff
main = mainImpl show

foreign import mainImpl :: (Number -> String) -> AlmostEff
