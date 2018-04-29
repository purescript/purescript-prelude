module Data.Semigroup (class Semigroup, append, (<>)) where

import Data.Internal.Record (unsafeGet, unsafeInsert)
import Data.RowList (RLProxy(..))
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

class SemigroupRecord rowlist row subrow focus | rowlist -> subrow focus where
  semigroupRecordImpl :: RLProxy rowlist -> Record row -> Record row -> Record subrow

instance semigroupRecordNil :: SemigroupRecord RL.Nil row () focus where
  semigroupRecordImpl _ _ _ = {}

instance semigroupRecordCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , SemigroupRecord rowlistTail row subrowTail subfocus
       , Semigroup focus
       )
    => SemigroupRecord (RL.Cons key focus rowlistTail) row subrow focus where
  semigroupRecordImpl _ ra rb
    = unsafeInsert key
        (unsafeGet' key ra <> unsafeGet' key rb)
        (semigroupRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb)
    where key = reflectSymbol (SProxy :: SProxy key)
          unsafeGet' = unsafeGet :: String -> Record row -> focus

instance semigroupRecord
    :: ( RL.RowToList row list
       , SemigroupRecord list row row focus
       )
    => Semigroup (Record row) where
  append = semigroupRecordImpl (RLProxy :: RLProxy list)
