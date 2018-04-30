module Data.Ring
  ( class Ring, sub, negate, (-)
  , module Data.Semiring

  , class RingRow
  , subRecordImpl
  ) where

import Data.Internal.Record (unsafeGet, unsafeInsert)
import Type.Data.RowList (RLProxy(..))
import Data.Semiring (class Semiring, class SemiringRow, add, mul, one, zero, (*), (+))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL

-- | The `Ring` class is for types that support addition, multiplication,
-- | and subtraction operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Additive inverse: `a - a = (zero - a) + a = zero`
class Semiring a <= Ring a where
  sub :: a -> a -> a

infixl 6 sub as -

instance ringInt :: Ring Int where
  sub = intSub

instance ringNumber :: Ring Number where
  sub = numSub

instance ringUnit :: Ring Unit where
  sub _ _ = unit

instance ringFn :: Ring b => Ring (a -> b) where
  sub f g x = f x - g x

-- | `negate x` can be used as a shorthand for `zero - x`.
negate :: forall a. Ring a => a -> a
negate a = zero - a

foreign import intSub :: Int -> Int -> Int
foreign import numSub :: Number -> Number -> Number

class RingRow rowlist row subrow focus | rowlist -> subrow focus where
  subRecordImpl  :: RLProxy rowlist -> Record row -> Record row -> Record subrow

instance ringRowNil :: RingRow RL.Nil row () focus where
  subRecordImpl  _ _ _ = {}

instance ringRowCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , RingRow rowlistTail row subrowTail subfocus
       , Ring focus
       )
    => RingRow (RL.Cons key focus rowlistTail) row subrow focus where
  subRecordImpl _ ra rb = insert (get ra - get rb) tail
    where
      insert = unsafeInsert key :: focus -> Record subrowTail -> Record subrow
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      tail = subRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb

instance ringRecord
    :: ( RL.RowToList row list
       , SemiringRow list row row focus
       , RingRow list row row focus
       )
    => Ring (Record row) where
  sub = subRecordImpl (RLProxy :: RLProxy list)
