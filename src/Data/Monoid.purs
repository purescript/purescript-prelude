module Data.Monoid
  ( class Monoid, mempty
  , power
  , guard
  , module Data.Semigroup
  ) where

import Data.Boolean (otherwise)
import Data.Eq ((==))
import Data.EuclideanRing (mod, (/))
import Data.Internal.Record (unsafeInsert)
import Data.Ord ((<=))
import Data.Ordering (Ordering(..))
import Data.RowList (RLProxy(..))
import Data.Semigroup (class Semigroup, class SemigroupRecord, (<>))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.RowList as RL

-- | A `Monoid` is a `Semigroup` with a value `mempty`, which is both a
-- | left and right unit for the associative operation `<>`:
-- |
-- | ```
-- | forall x. mempty <> x = x <> mempty = x
-- | ```
-- |
-- | `Monoid`s are commonly used as the result of fold operations, where
-- | `<>` is used to combine individual results, and `mempty` gives the result
-- | of folding an empty collection of elements.
class Semigroup m <= Monoid m where
  mempty :: m

instance monoidUnit :: Monoid Unit where
  mempty = unit

instance monoidOrdering :: Monoid Ordering where
  mempty = EQ

instance monoidFn :: Monoid b => Monoid (a -> b) where
  mempty _ = mempty

instance monoidString :: Monoid String where
  mempty = ""

instance monoidArray :: Monoid (Array a) where
  mempty = []

class MonoidRecord rowlist row focus | rowlist -> row focus where
  monoidRecordImpl :: RLProxy rowlist -> Record row

instance monoidRecordNil :: MonoidRecord RL.Nil () focus where
  monoidRecordImpl _ = {}

instance monoidRecordCons
    :: ( IsSymbol key
       , Monoid focus
       , MonoidRecord rowlistTail row subfocus
       )
    => MonoidRecord (RL.Cons key focus rowlistTail) row focus where
  monoidRecordImpl _
    = unsafeInsert key
        (mempty :: focus)
        (monoidRecordImpl (RLProxy :: RLProxy rowlistTail))
    where
      key = reflectSymbol (SProxy :: SProxy key)

instance monoidRecord
    :: ( RL.RowToList row list
       , SemigroupRecord list row row focus
       , MonoidRecord list row focus
       )
    => Monoid (Record row) where
  mempty = monoidRecordImpl (RLProxy :: RLProxy list)


-- | Append a value to itself a certain number of times. For the
-- | `Multiplicative` type, and for a non-negative power, this is the same as
-- | normal number exponentiation.
-- |
-- | If the second argument is negative this function will return `mempty`
-- | (*unlike* normal number exponentiation). The `Monoid` constraint alone
-- | is not enough to write a `power` function with the property that `power x
-- | n` cancels with `power x (-n)`, i.e. `power x n <> power x (-n) = mempty`.
-- | For that, we would additionally need the ability to invert elements, i.e.
-- | a Group.
power :: forall m. Monoid m => m -> Int -> m
power x = go
  where
  go :: Int -> m
  go p
    | p <= 0 = mempty
    | p == 1 = x
    | p `mod` 2 == 0 = let x' = go (p / 2) in x' <> x'
    | otherwise = let x' = go (p / 2) in x' <> x' <> x

-- | Allow or "truncate" a Monoid to its `mempty` value based on a condition.
guard :: forall m. Monoid m => Boolean -> m -> m
guard true a = a
guard false _ = mempty
