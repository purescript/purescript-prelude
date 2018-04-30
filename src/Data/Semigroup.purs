module Data.Semigroup
  ( class Semigroup
  , append
  , (<>)

  , class SemigroupRow
  , appendRecordImpl
  ) where

import Data.Internal.Record (unsafeGet, unsafeInsert)
import Type.Data.RowList (RLProxy(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)
import Prim.Row as Row
import Prim.RowList as RL

-- | The `Semigroup` type class identifies an associative operation on a type.
-- |
-- | Instances are required to satisfy the following law:
-- |
-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
-- |
-- | One example of a `Semigroup` is `String`, with `(<>)` defined as string
-- | concatenation.
class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

instance semigroupString :: Semigroup String where
  append = concatString

instance semigroupUnit :: Semigroup Unit where
  append _ _ = unit

instance semigroupVoid :: Semigroup Void where
  append _ = absurd

instance semigroupFn :: Semigroup s' => Semigroup (s -> s') where
  append f g x = f x <> g x

instance semigroupArray :: Semigroup (Array a) where
  append = concatArray

foreign import concatString :: String -> String -> String
foreign import concatArray :: forall a. Array a -> Array a -> Array a

class SemigroupRow rowlist row subrow focus | rowlist -> subrow focus where
  appendRecordImpl :: RLProxy rowlist -> Record row -> Record row -> Record subrow

instance semigroupRowNil :: SemigroupRow RL.Nil row () focus where
  appendRecordImpl _ _ _ = {}

instance semigroupRecordCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , SemigroupRow rowlistTail row subrowTail subfocus
       , Semigroup focus
       )
    => SemigroupRow (RL.Cons key focus rowlistTail) row subrow focus where
  appendRecordImpl _ ra rb = insert (get ra <> get rb) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeInsert key :: focus -> Record subrowTail -> Record subrow
      tail = appendRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb

instance semigroupRow
    :: ( RL.RowToList row list
       , SemigroupRow list row row focus
       )
    => Semigroup (Record row) where
  append = appendRecordImpl (RLProxy :: RLProxy list)
