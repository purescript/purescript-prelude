module Data.Ring.Laws where

import Prelude

-- | `a - a = (zero - a) + a = zero`
additiveInverse :: forall a. Eq a => Ring a => a -> Boolean
additiveInverse a = a - a == (zero - a) + a && a - a == zero
