module Data.Semiring
  ( Semiring
  , add
  , zero
  , mul
  , one
  , (+)
  , (*)
  ) where

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
-- | - Annihiliation: `zero * a = a * zero = zero`
class Semiring a where
  add  :: a -> a -> a
  zero :: a
  mul  :: a -> a -> a
  one  :: a

instance semiringFn :: (Semiring b) => Semiring (a -> b) where
  add f g x = f x `add` g x
  zero _ = zero
  mul f g x = f x `mul` g x
  one _ = one

instance semiringInt :: Semiring Int where
  add = intAdd
  zero = 0
  mul = intMul
  one = 1

instance semiringNumber :: Semiring Number where
  add = numAdd
  zero = 0.0
  mul = numMul
  one = 1.0

infixl 6 +
infixl 7 *

-- | `(+)` is an alias for `add`.
(+) :: forall a. (Semiring a) => a -> a -> a
(+) = add

-- | `(*)` is an alias for `mul`.
(*) :: forall a. (Semiring a) => a -> a -> a
(*) = mul

foreign import intAdd :: Int -> Int -> Int
foreign import intMul :: Int -> Int -> Int
foreign import numAdd :: Number -> Number -> Number
foreign import numMul :: Number -> Number -> Number
