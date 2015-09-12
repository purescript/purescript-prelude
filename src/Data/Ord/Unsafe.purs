module Data.Ord.Unsafe (nativeCompare) where

import Data.Ordering (Ordering(..))

nativeCompare :: forall a. a -> a -> Ordering
nativeCompare = nativeCompareImpl LT EQ GT

foreign import nativeCompareImpl :: forall a. Ordering -> Ordering -> Ordering -> a -> a -> Ordering
