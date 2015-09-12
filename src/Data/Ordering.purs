module Data.Ordering where

import Data.Eq (Eq)
import Data.Bounded (Bounded)
import Data.Show (Show)
import Data.Semigroup (Semigroup)

-- | The `Ordering` data type represents the three possible outcomes of
-- | comparing two values:
-- |
-- | `LT` - The first value is _less than_ the second.
-- | `GT` - The first value is _greater than_ the second.
-- | `EQ` - The first value is _equal to_ the second.
data Ordering = LT | GT | EQ

instance eqOrdering :: Eq Ordering where
  eq LT LT = true
  eq GT GT = true
  eq EQ EQ = true
  eq _  _  = false

instance boundedOrdering :: Bounded Ordering where
  top = GT
  bottom = LT

instance semigroupOrdering :: Semigroup Ordering where
  append LT _ = LT
  append GT _ = GT
  append EQ y = y

instance showOrdering :: Show Ordering where
  show LT = "LT"
  show GT = "GT"
  show EQ = "EQ"
