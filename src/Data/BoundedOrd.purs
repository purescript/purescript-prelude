module Data.BoundedOrd
  ( BoundedOrd
  , module Data.Bounded
  , module Data.Ord
  ) where

import Data.Bounded
import Data.Ord

-- | The `BoundedOrd` type class represents totally ordered finite data types.
-- |
-- | Instances should satisfy the following law in addition to the `Ord` laws:
-- |
-- | - Ordering: `bottom <= a <= top`
class (Bounded a, Ord a) <= BoundedOrd a

instance boundedOrdBoolean :: BoundedOrd Boolean
instance boundedOrdInt :: BoundedOrd Int
instance boundedOrdChar :: BoundedOrd Char
instance boundedOrdOrdering :: BoundedOrd Ordering
