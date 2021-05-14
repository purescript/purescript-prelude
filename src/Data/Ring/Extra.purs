module Data.Ring.Extra where

import Data.Ord ((>=))
import Data.Ring (class Ring, one, negate)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))

fromInt :: forall a. Ring a => Int -> a
fromInt x =
  if x >= 0
    then (\(Additive i) -> i) (power (Additive one) x)
    else negate (fromInt (negate x))