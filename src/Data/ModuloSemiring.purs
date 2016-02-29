module Data.ModuloSemiring
  ( class ModuloSemiring, div, mod, (/)
  , module Data.Semiring
  ) where

import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Unit (Unit, unit)
import Data.Void (Void)

-- | The `ModuloSemiring` class is for types that support addition,
-- | multiplication, division, and modulo (division remainder) operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Remainder: ``a / b * b + (a `mod` b) = a``
class Semiring a <= ModuloSemiring a where
  div :: a -> a -> a
  mod :: a -> a -> a

infixl 7 div as /

instance moduloSemiringInt :: ModuloSemiring Int where
  div = intDiv
  mod = intMod

instance moduloSemiringNumber :: ModuloSemiring Number where
  div = numDiv
  mod _ _ = 0.0

instance moduloSemiringUnit :: ModuloSemiring Unit where
  div _ _ = unit
  mod _ _ = unit

foreign import intDiv :: Int -> Int -> Int
foreign import numDiv :: Number -> Number -> Number
foreign import intMod :: Int -> Int -> Int
