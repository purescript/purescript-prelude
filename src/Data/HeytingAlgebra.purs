module Data.HeytingAlgebra
  ( class HeytingAlgebra

  , tt
  , ff
  , implies
  , conj
  , disj
  , not
  , (&&)
  , (||)

  , class HeytingAlgebraRecord
  , ffRecordImpl
  , ttRecordImpl
  , impliesRecordImpl
  , conjRecordImpl
  , disjRecordImpl
  , notRecordImpl
  ) where

import Data.Internal.Record (unsafeGet, unsafeInsert)
import Data.RowList (RLProxy(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Data.Row (RProxy(..))

-- | The `HeytingAlgebra` type class represents types that are bounded lattices with
-- | an implication operator such that the following laws hold:
-- |
-- | - Associativity:
-- |   - `a || (b || c) = (a || b) || c`
-- |   - `a && (b && c) = (a && b) && c`
-- | - Commutativity:
-- |   - `a || b = b || a`
-- |   - `a && b = b && a`
-- | - Absorption:
-- |   - `a || (a && b) = a`
-- |   - `a && (a || b) = a`
-- | - Idempotent:
-- |   - `a || a = a`
-- |   - `a && a = a`
-- | - Identity:
-- |   - `a || ff = a`
-- |   - `a && tt = a`
-- | - Implication:
-- |   - ``a `implies` a = tt``
-- |   - ``a && (a `implies` b) = a && b``
-- |   - ``b && (a `implies` b) = b``
-- |   - ``a `implies` (b && c) = (a `implies` b) && (a `implies` c)``
-- | - Complemented:
-- |   - ``not a = a `implies` ff``
class HeytingAlgebra a where
  ff :: a
  tt :: a
  implies :: a -> a -> a
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

infixr 3 conj as &&
infixr 2 disj as ||

instance heytingAlgebraBoolean :: HeytingAlgebra Boolean where
  ff = false
  tt = true
  implies a b = not a || b
  conj = boolConj
  disj = boolDisj
  not = boolNot

instance heytingAlgebraUnit :: HeytingAlgebra Unit where
  ff = unit
  tt = unit
  implies _ _ = unit
  conj _ _ = unit
  disj _ _ = unit
  not _ = unit

instance heytingAlgebraFunction :: HeytingAlgebra b => HeytingAlgebra (a -> b) where
  ff _ = ff
  tt _ = tt
  implies f g a = f a `implies` g a
  conj f g a = f a && g a
  disj f g a = f a || g a
  not f a = not (f a)

foreign import boolConj :: Boolean -> Boolean -> Boolean
foreign import boolDisj :: Boolean -> Boolean -> Boolean
foreign import boolNot :: Boolean -> Boolean

class HeytingAlgebraRecord rowlist row subrow focus | rowlist -> subrow focus where
  ffRecordImpl :: RLProxy rowlist -> RProxy row -> Record subrow
  ttRecordImpl :: RLProxy rowlist -> RProxy row -> Record subrow
  impliesRecordImpl :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  disjRecordImpl :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  conjRecordImpl :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  notRecordImpl :: RLProxy rowlist -> Record row -> Record subrow

instance heytingAlgebraRecordNil :: HeytingAlgebraRecord RL.Nil row () focus where
  conjRecordImpl _ _ _ = {}
  disjRecordImpl _ _ _ = {}
  ffRecordImpl _ _ = {}
  impliesRecordImpl _ _ _ = {}
  notRecordImpl _ _ = {}
  ttRecordImpl _ _ = {}

instance heytingAlgebraRecordCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , HeytingAlgebraRecord rowlistTail row subrowTail subfocus
       , HeytingAlgebra focus
       )
    => HeytingAlgebraRecord (RL.Cons key focus rowlistTail) row subrow focus where
  conjRecordImpl _ ra rb
    = unsafeInsert key
        (conj (unsafeGet' key ra) (unsafeGet' key rb))
        (conjRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb)
    where key = reflectSymbol (SProxy :: SProxy key)
          unsafeGet' = unsafeGet :: String -> Record row -> focus

  disjRecordImpl _ ra rb
    = unsafeInsert key
        (disj (unsafeGet' key ra) (unsafeGet' key rb))
        (disjRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb)
    where key = reflectSymbol (SProxy :: SProxy key)
          unsafeGet' = unsafeGet :: String -> Record row -> focus

  impliesRecordImpl _ ra rb
    = unsafeInsert key
        (implies (unsafeGet' key ra) (unsafeGet' key rb))
        (impliesRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb)
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus

  ffRecordImpl _ _
    = unsafeInsert key (ff :: focus)
        ( ffRecordImpl
            (RLProxy :: RLProxy rowlistTail)
            (RProxy :: RProxy row)
        )
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus

  notRecordImpl _ row
    = unsafeInsert key (not (unsafeGet' key row))
        (notRecordImpl (RLProxy :: RLProxy rowlistTail) row)
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus

  ttRecordImpl _ _
    = unsafeInsert key (tt :: focus)
        ( ttRecordImpl
            (RLProxy :: RLProxy rowlistTail)
            (RProxy :: RProxy row)
        )
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus

instance heytingAlgebraRecord
    :: ( RL.RowToList row list
       , HeytingAlgebraRecord list row row focus
       )
    => HeytingAlgebra (Record row) where
  ff = ffRecordImpl  (RLProxy :: RLProxy list) (RProxy :: RProxy row)
  tt = ttRecordImpl  (RLProxy :: RLProxy list) (RProxy :: RProxy row)
  conj = conjRecordImpl  (RLProxy :: RLProxy list)
  disj = conjRecordImpl  (RLProxy :: RLProxy list)
  implies = conjRecordImpl  (RLProxy :: RLProxy list)
  not = notRecordImpl  (RLProxy :: RLProxy list)
