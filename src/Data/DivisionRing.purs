module Data.DivisionRing
  ( class DivisionRing
  , module Data.ModuloSemiring
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.ModuloSemiring (class ModuloSemiring, div, mod, (/))
import Data.Ring (class Ring, negate, sub)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Unit (Unit)

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

instance divisionRingNumber :: DivisionRing Number
instance divisionRingUnit :: DivisionRing Unit
