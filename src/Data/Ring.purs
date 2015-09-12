module Data.Ring
  ( Ring
  , sub
  , (-)
  , negate
  , module Data.Semiring
  ) where

import Data.Semiring

-- | The `Ring` class is for types that support addition, multiplication,
-- | and subtraction operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Additive inverse: `a - a = (zero - a) + a = zero`
class (Semiring a) <= Ring a where
  sub :: a -> a -> a

instance ringFn :: (Ring b) => Ring (a -> b) where
  sub f g x = f x `sub` g x

instance ringInt :: Ring Int where
  sub = intSub

instance ringNumber :: Ring Number where
  sub = numSub

infixl 6 -

-- | `(-)` is an alias for `sub`.
(-) :: forall a. (Ring a) => a -> a -> a
(-) = sub

-- | `negate x` can be used as a shorthand for `zero - x`.
negate :: forall a. (Ring a) => a -> a
negate a = zero - a

foreign import intSub :: Int -> Int -> Int
foreign import numSub :: Number -> Number -> Number
