module Data.Ord
  ( Ord
  , compare
  , (<)
  , (>)
  , (<=)
  , (>=)
  , module Data.Eq
  , module Data.Ordering
  ) where

import Data.Eq
import Data.Ord.Unsafe (nativeCompare)
import Data.Ordering

-- | The `Ord` type class represents types which support comparisons with a
-- | _total order_.
-- |
-- | `Ord` instances should satisfy the laws of total orderings:
-- |
-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering

instance ordOrdering :: Ord Ordering where
  compare LT LT = EQ
  compare EQ EQ = EQ
  compare GT GT = EQ
  compare LT _  = LT
  compare EQ LT = GT
  compare EQ GT = LT
  compare GT _  = GT

instance ordBoolean :: Ord Boolean where
  compare = nativeCompare

instance ordInt :: Ord Int where
  compare = nativeCompare

instance ordNumber :: Ord Number where
  compare = nativeCompare

instance ordString :: Ord String where
  compare = nativeCompare

instance ordChar :: Ord Char where
  compare = nativeCompare

instance ordArray :: (Ord a) => Ord (Array a) where
  compare = ordArrayImpl LT EQ GT compare

infixl 4 <
infixl 4 >
infixl 4 <=
infixl 4 >=

-- | Test whether one value is _strictly less than_ another.
(<) :: forall a. (Ord a) => a -> a -> Boolean
(<) x y = compare x y == LT

-- | Test whether one value is _strictly greater than_ another.
(>) :: forall a. (Ord a) => a -> a -> Boolean
(>) x y = compare x y == GT

-- | Test whether one value is _non-strictly less than_ another.
(<=) :: forall a. (Ord a) => a -> a -> Boolean
(<=) x y = compare x y /= GT

-- | Test whether one value is _non-strictly greater than_ another.
(>=) :: forall a. (Ord a) => a -> a -> Boolean
(>=) x y = compare x y /= LT

foreign import ordArrayImpl :: forall a. Ordering -> Ordering -> Ordering -> (a -> a -> Ordering) -> Array a -> Array a -> Ordering
