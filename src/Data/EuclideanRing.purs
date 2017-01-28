module Data.EuclideanRing
  ( class EuclideanRing, degree, div, mod, (/)
  , module Data.CommutativeRing
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.CommutativeRing (class CommutativeRing)
import Data.Ring (class Ring, sub, (-))
import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))

-- | The `EuclideanRing` class is for commutative rings that support division.
-- | The mathematical structure this class is based on is also called
-- | a *Euclidean domain*.
-- |
-- | Instances must satisfy the following law in addition to the `Ring`
-- | laws:
-- |
-- | - Integral domain: `one /= zero`, and if both `a /= 0` and `b /= 0` then `a * b /= 0`
-- | - Multiplicative Euclidean function: ``a = (a / b) * b + (a `mod` b)``
-- |   where `degree a > 0` and `degree a <= degree (a * b)`
class CommutativeRing a <= EuclideanRing a where
  degree :: a -> Int
  div :: a -> a -> a
  mod :: a -> a -> a

infixl 7 div as /

instance euclideanRingInt :: EuclideanRing Int where
  degree = intDegree
  div = intDiv
  mod = intMod

instance euclideanRingNumber :: EuclideanRing Number where
  degree _ = 1
  div = numDiv
  mod _ _ = 0.0

foreign import intDegree :: Int -> Int
foreign import intDiv :: Int -> Int -> Int
foreign import intMod :: Int -> Int -> Int

foreign import numDiv :: Number -> Number -> Number
