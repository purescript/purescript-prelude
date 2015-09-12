module Data.Eq
  ( Eq
  , eq
  , (==)
  , (/=)
  ) where

import Data.BooleanAlgebra (not)
import Data.Eq.Unsafe (nativeEq)

-- | The `Eq` type class represents types which support decidable equality.
-- |
-- | `Eq` instances should satisfy the following laws:
-- |
-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
class Eq a where
  eq :: a -> a -> Boolean

instance eqBoolean :: Eq Boolean where
  eq = nativeEq

instance eqInt :: Eq Int where
  eq = nativeEq

instance eqNumber :: Eq Number where
  eq = nativeEq

instance eqChar :: Eq Char where
  eq = nativeEq

instance eqString :: Eq String where
  eq = nativeEq

instance eqArray :: (Eq a) => Eq (Array a) where
  eq = arrayEq eq

infix 4 ==
infix 4 /=

-- | `(==)` is an alias for `eq`. Tests whether one value is equal to another.
(==) :: forall a. (Eq a) => a -> a -> Boolean
(==) = eq

-- | `(/=)` tests whether one value is _not equal_ to another. Shorthand for
-- | `not (x == y)`.
(/=) :: forall a. (Eq a) => a -> a -> Boolean
(/=) x y = not (x == y)

foreign import arrayEq :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Boolean
