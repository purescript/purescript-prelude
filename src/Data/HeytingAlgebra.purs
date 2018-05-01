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

  , class HeytingAlgebraRow
  , ffRecordImpl
  , ttRecordImpl
  , impliesRecordImpl
  , conjRecordImpl
  , disjRecordImpl
  , notRecordImpl
  ) where

import Data.Internal.Record (unsafeGet, unsafeInsert)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))

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

class HeytingAlgebraRow rowlist row subrow focus | rowlist -> subrow focus where
  ffRecordImpl :: RLProxy rowlist -> RProxy row -> Record subrow
  ttRecordImpl :: RLProxy rowlist -> RProxy row -> Record subrow
  impliesRecordImpl :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  disjRecordImpl :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  conjRecordImpl :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  notRecordImpl :: RLProxy rowlist -> Record row -> Record subrow

instance heytingAlgebraRowNil :: HeytingAlgebraRow RL.Nil row () focus where
  conjRecordImpl _ _ _ = {}
  disjRecordImpl _ _ _ = {}
  ffRecordImpl _ _ = {}
  impliesRecordImpl _ _ _ = {}
  notRecordImpl _ _ = {}
  ttRecordImpl _ _ = {}

instance heytingAlgebraRowCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , HeytingAlgebraRow rowlistTail row subrowTail subfocus
       , HeytingAlgebra focus
       )
    => HeytingAlgebraRow (RL.Cons key focus rowlistTail) row subrow focus where
  conjRecordImpl _ ra rb = insert (conj (get ra) (get rb)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeInsert key :: focus -> Record subrowTail -> Record subrow
      tail = conjRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb

  disjRecordImpl _ ra rb = insert (disj (get ra) (get rb)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeInsert key :: focus -> Record subrowTail -> Record subrow
      tail = disjRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb

  impliesRecordImpl _ ra rb = insert (implies (get ra) (get rb)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeInsert key :: focus -> Record subrowTail -> Record subrow
      tail = impliesRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb

  ffRecordImpl _ row = insert ff tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      insert = unsafeInsert key :: focus -> Record subrowTail -> Record subrow
      tail = ffRecordImpl (RLProxy :: RLProxy rowlistTail) row

  notRecordImpl _ row
    = insert (not (get row)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeInsert key :: focus -> Record subrowTail -> Record subrow
      tail = notRecordImpl (RLProxy :: RLProxy rowlistTail) row

  ttRecordImpl _ row = insert tt tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      insert = unsafeInsert key :: focus -> Record subrowTail -> Record subrow
      tail = ttRecordImpl (RLProxy :: RLProxy rowlistTail) row

instance heytingAlgebraRecord
    :: ( RL.RowToList row list
       , HeytingAlgebraRow list row row focus
       )
    => HeytingAlgebra (Record row) where
  ff = ffRecordImpl  (RLProxy :: RLProxy list) (RProxy :: RProxy row)
  tt = ttRecordImpl  (RLProxy :: RLProxy list) (RProxy :: RProxy row)
  conj = conjRecordImpl  (RLProxy :: RLProxy list)
  disj = disjRecordImpl  (RLProxy :: RLProxy list)
  implies = impliesRecordImpl  (RLProxy :: RLProxy list)
  not = notRecordImpl  (RLProxy :: RLProxy list)
