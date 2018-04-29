module Data.Bounded
  ( class Bounded
  , bottom
  , top
  , module Data.Ord
  ) where

import Data.Ord (class Ord, Ordering(..), compare, (<), (<=), (>), (>=))
import Data.Unit (Unit, unit)

-- | The `Bounded` type class represents totally ordered types that have an
-- | upper and lower boundary.
-- |
-- | Instances should satisfy the following law in addition to the `Ord` laws:
-- |
-- | - Bounded: `bottom <= a <= top`
class Ord a <= Bounded a where
  top :: a
  bottom :: a

instance boundedBoolean :: Bounded Boolean where
  top = true
  bottom = false

instance boundedInt :: Bounded Int where
  top = topInt
  bottom = bottomInt

foreign import topInt :: Int
foreign import bottomInt :: Int

-- | Characters fall within the Unicode range.
instance boundedChar :: Bounded Char where
  top = topChar
  bottom = bottomChar

foreign import topChar :: Char
foreign import bottomChar :: Char

instance boundedOrdering :: Bounded Ordering where
  top = GT
  bottom = LT

instance boundedUnit :: Bounded Unit where
  top = unit
  bottom = unit

foreign import topNumber :: Number
foreign import bottomNumber :: Number

instance boundedNumber :: Bounded Number where
  top = topNumber
  bottom = bottomNumber

-- Similarly to the `OrdRecord` constraint, this implementation is potentially
-- unstable. However, it is left here as a reference:

--class BoundedRecord rowlist row subrow focus | rowlist -> subrow focus where
--  bottomRecordImpl :: RLProxy rowlist -> RProxy row -> Record subrow
--  topRecordImpl :: RLProxy rowlist -> RProxy row -> Record subrow
--
--instance boundedRecordNil :: BoundedRecord RL.Nil row () focus where
--  bottomRecordImpl _ _ = {}
--  topRecordImpl _ _ = {}
--
--instance boundedRecordCons
--    :: ( BoundedRecord rowlistTail row subrowTail subfocus
--       , Row.Cons key focus subrowTail subrow
--       , IsSymbol key
--       , Bounded focus
--       )
--    => BoundedRecord (Row.Cons key focus rowlistTail) row subrow focus where
--  bottomRecordImpl _ row
--    = unsafeInsert key (bottom :: focus)
--        (bottomRecordImpl (RLProxy :: RLProxy rowlistTail) row)
--    where key = reflectSymbol (SProxy :: SProxy key)
--
--  topRecordImpl _ row
--    = unsafeInsert key (bottom :: focus)
--        (topRecordImpl (RLProxy :: RLProxy rowlistTail) row)
--    where key = reflectSymbol (SProxy :: SProxy key)
--
--instance boundedRecord
--    :: ( RL.RowToList row list
--       , BoundedRecord list row row focus
--       , OrdRecord list row row focus
--       )
--    => Bounded (Record row) where
--  bottom = bottomRecordImpl (RLProxy :: RLProxy list) (RProxy :: RProxy row)
--  top = topRecordImpl (RLProxy :: RLProxy list) (RProxy :: RProxy row)
