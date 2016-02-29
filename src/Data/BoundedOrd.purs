module Data.BoundedOrd
  ( class BoundedOrd
  , module Data.Bounded
  , module Data.Ord
  ) where

import Data.Bounded (class Bounded, bottom, top)
import Data.Ord (class Ord, Ordering(..), compare, (<), (<=), (>), (>=))
import Data.Unit (Unit)

-- | The `BoundedOrd` type class represents types with an upper and lower
-- | boundary and that are totally ordered.
-- |
-- | Instances should satisfy the following law in addition to the `Ord` laws:
-- |
-- | - Ordering: `bottom <= a <= top`
class (Bounded a, Ord a) <= BoundedOrd a

instance boundedOrdBoolean :: BoundedOrd Boolean where
instance boundedOrdInt :: BoundedOrd Int where
instance boundedOrdChar :: BoundedOrd Char where
instance boundedOrdUnit :: BoundedOrd Unit where
instance boundedOrdOrdering :: BoundedOrd Ordering where

