module Data.ModuloSemiring
  ( ModuloSemiring
  , div
  , mod
  , (/)
  , module Data.Semiring
  ) where

import Data.Semiring

-- | The `ModuloSemiring` class is for types that support addition,
-- | multiplication, division, and modulo (division remainder) operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Remainder: ``a / b * b + (a `mod` b) = a``
class (Semiring a) <= ModuloSemiring a where
  div :: a -> a -> a
  mod :: a -> a -> a

instance semiringFn :: (ModuloSemiring b) => ModuloSemiring (a -> b) where
  div f g x = f x `div` g x
  mod f g x = f x `mod` g x

instance moduloSemiringInt :: ModuloSemiring Int where
  div = divIntImpl
  mod = modIntImpl

instance moduloSemiringNumber :: ModuloSemiring Number where
  div = divNumberImpl
  mod _ _ = 0.0

infixl 7 /

-- | `(/)` is an alias for `div`.
(/) :: forall a. (ModuloSemiring a) => a -> a -> a
(/) = div

foreign import divIntImpl :: Int -> Int -> Int
foreign import modIntImpl :: Int -> Int -> Int
foreign import divNumberImpl :: Number -> Number -> Number
