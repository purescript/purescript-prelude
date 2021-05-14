module Data.BooleanAlgebra.Laws where

import Prelude

import Data.BooleanAlgebra (tt)

-- | `a || not a = tt`
excludedMiddle :: forall a. Eq a => BooleanAlgebra a => a -> Boolean
excludedMiddle a = (a || not a) == tt
