module Data.Field
  ( class Field
  , module Data.DivisionRing
  , module Data.CommutativeRing
  , module Data.EuclideanRing
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.DivisionRing (class DivisionRing, recip)
import Data.CommutativeRing (class CommutativeRing)
import Data.EuclideanRing (class EuclideanRing, degree, div, mod, (/), gcd, lcm)
import Data.Ring (class Ring, negate, sub)
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))

-- | The `Field` class is for types that are (commutative) fields.
-- |
-- | Instances must satisfy the following law in addition to the
-- | `EuclideanRing` laws:
-- |
-- | - Non-zero multiplicative inverse: ``a `mod` b = zero`` for all `a` and `b`
-- |
-- | If a type has a `Field` instance, it should also have a `DivisionRing`
-- | instance. In a future release, `DivisionRing` may become a superclass of
-- | `Field`.
class EuclideanRing a <= Field a

instance fieldNumber :: Field Number
