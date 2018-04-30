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
-- | `Field`s are exactly `EuclideanRing` + `CommutativeRing` so this class
-- | exists as a convenience, so a single constraint can be used when field-like
-- | behaviour is expected.
class (EuclideanRing a, CommutativeRing a) <= Field a

instance fieldNumber :: (EuclideanRing a, CommutativeRing a) => Field a
