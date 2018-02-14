module Data.Ring
  ( class Ring, sub, negate, (-)
  , module Data.Semiring
  ) where

import Data.Semiring (class Semiring, add, mul, one, zero, (*), (+))
import Data.Unit (Unit, unit)

-- | The `Ring` class is for types that support addition, multiplication,
-- | and subtraction operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Additive inverse: `a - a = (zero - a) + a = zero`
class Semiring a <= Ring a where
  sub :: a -> a -> a

infixl 6 sub as -

instance ringInt :: Ring Int where
  sub = intSub

instance ringUInt :: Ring UInt where
  sub = uintSub

instance ringNumber :: Ring Number where
  sub = numSub

instance ringUnit :: Ring Unit where
  sub _ _ = unit

instance ringFn :: Ring b => Ring (a -> b) where
  sub f g x = f x - g x

-- | `negate x` can be used as a shorthand for `zero - x`.
negate :: forall a. Ring a => a -> a
negate a = zero - a

foreign import intSub :: Int -> Int -> Int
foreign import uintSub :: UInt -> UInt -> UInt
foreign import numSub :: Number -> Number -> Number
