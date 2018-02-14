module Data.Semiring (class Semiring, add, (+), zero, mul, (*), one) where

import Data.Unit (Unit, unit)

-- | The `Semiring` class is for types that support an addition and
-- | multiplication operation.
-- |
-- | Instances must satisfy the following laws:
-- |
-- | - Commutative monoid under addition:
-- |   - Associativity: `(a + b) + c = a + (b + c)`
-- |   - Identity: `zero + a = a + zero = a`
-- |   - Commutative: `a + b = b + a`
-- | - Monoid under multiplication:
-- |   - Associativity: `(a * b) * c = a * (b * c)`
-- |   - Identity: `one * a = a * one = a`
-- | - Multiplication distributes over addition:
-- |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
-- |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
-- | - Annihilation: `zero * a = a * zero = zero`
-- |
-- | **Note:** The `Number` and `Int` types are not fully law abiding
-- | members of this class hierarchy due to the potential for arithmetic
-- | overflows, and in the case of `Number`, the presence of `NaN` and
-- | `Infinity` values. The behaviour is unspecified in these cases.
class Semiring a where
  add  :: a -> a -> a
  zero :: a
  mul  :: a -> a -> a
  one  :: a

infixl 6 add as +
infixl 7 mul as *

instance semiringInt :: Semiring Int where
  add = intAdd
  zero = 0
  mul = intMul
  one = 1

instance semiringUInt :: Semiring UInt where
  add = uintAdd
  zero = 0u
  mul = uintMul
  one = 1u

instance semiringNumber :: Semiring Number where
  add = numAdd
  zero = 0.0
  mul = numMul
  one = 1.0

instance semiringFn :: Semiring b => Semiring (a -> b) where
  add f g x = f x + g x
  zero = \_ -> zero
  mul f g x = f x * g x
  one = \_ -> one

instance semiringUnit :: Semiring Unit where
  add _ _ = unit
  zero = unit
  mul _ _ = unit
  one = unit

foreign import intAdd :: Int -> Int -> Int
foreign import intMul :: Int -> Int -> Int
foreign import uintAdd :: UInt -> UInt -> UInt
foreign import uintMul :: UInt -> UInt -> UInt
foreign import numAdd :: Number -> Number -> Number
foreign import numMul :: Number -> Number -> Number
