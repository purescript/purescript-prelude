module Data.DivisionRing
  ( DivisionRing
  , module Data.Ring
  , module Data.ModuloSemiring
  ) where

import Data.Ring
import Data.ModuloSemiring

-- | A `Ring` where every nonzero element has a multiplicative inverse.
-- |
-- | Instances must satisfy the following law in addition to the `Ring` and
-- | `ModuloSemiring` laws:
-- |
-- | - Multiplicative inverse: `(one / x) * x = one`
-- |
-- | As a consequence of this ```a `mod` b = zero``` as no divide operation
-- | will have a remainder.
class (Ring a, ModuloSemiring a) <= DivisionRing a

instance divisionRingFn :: (DivisionRing b) => DivisionRing (a -> b)
instance divisionRingNumber :: DivisionRing Number
