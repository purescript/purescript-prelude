module Data.CommutativeRing
  ( class CommutativeRing
  , module Data.Ring
  , module Data.Semiring
  ) where

import Data.Ring (class Ring, class RingRow)
import Data.Semiring (class Semiring, class SemiringRow, add, mul, one, zero, (*), (+))
import Data.Unit (Unit)
import Prim.RowList as RL

-- | The `CommutativeRing` class is for rings where multiplication is
-- | commutative.
-- |
-- | Instances must satisfy the following law in addition to the `Ring`
-- | laws:
-- |
-- | - Commutative multiplication: `a * b = b * a`
class Ring a <= CommutativeRing a

instance commutativeRingInt :: CommutativeRing Int
instance commutativeRingNumber :: CommutativeRing Number
instance commutativeRingUnit :: CommutativeRing Unit
instance commutativeRingFn :: CommutativeRing b => CommutativeRing (a -> b)

instance commutativeRingRecord
  :: ( RL.RowToList row list
     , SemiringRow list row row focus
     , RingRow list row row focus
     )
  => CommutativeRing (Record row)
