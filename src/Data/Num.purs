module Data.Num
  ( class Num
  , module Data.DivisionRing
  , module Data.ModuloSemiring
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.ModuloSemiring (class ModuloSemiring, div, mod, (/))
import Data.Ring (class Ring, negate, sub)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.DivisionRing (class DivisionRing)
import Data.Unit (Unit)

-- | The `Num` class is for types that are commutative fields.
-- |
-- | Instances must satisfy the following law in addition to the
-- | `DivisionRing` laws:
-- |
-- | - Commutative multiplication: `a * b = b * a`
class DivisionRing a <= Num a

instance numNumber :: Num Number
instance numUnit :: Num Unit
