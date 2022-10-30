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
  , H1Record
  , H2Record
  , class HeytingAlgebraRecord
  , ffRecord
  , ttRecord
  , impliesRecord
  , conjRecord
  , disjRecord
  , notRecord
  ) where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))

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

instance heytingAlgebraProxy :: HeytingAlgebra (Proxy a) where
  conj _ _ = Proxy
  disj _ _ = Proxy
  implies _ _ = Proxy
  ff = Proxy
  not _ = Proxy
  tt = Proxy

instance heytingAlgebraRecord :: (RL.RowToList row list, HeytingAlgebraRecord list row row) => HeytingAlgebra (Record row) where
  ff = unH1Record (ffRecord :: H1Record list row row)
  tt = unH1Record (ttRecord :: H1Record list row row)
  conj l = conjRecord (H2Record l :: H2Record list row)
  disj l = disjRecord (H2Record l :: H2Record list row)
  implies l = impliesRecord (H2Record l :: H2Record list row)
  not l = notRecord (H2Record l :: H2Record list row)

foreign import boolConj :: Boolean -> Boolean -> Boolean
foreign import boolDisj :: Boolean -> Boolean -> Boolean
foreign import boolNot :: Boolean -> Boolean

newtype H1Record :: RL.RowList Type -> Row Type -> Row Type -> Type
newtype H1Record rowlist row subrow = H1Record { | subrow }

unH1Record :: forall rowlist row subrow. H1Record rowlist row subrow -> { | subrow }
unH1Record (H1Record r) = r

newtype H2Record :: RL.RowList Type -> Row Type -> Type
newtype H2Record rowlist row = H2Record { | row }

-- | A class for records where all fields have `HeytingAlgebra` instances, used
-- | to implement the `HeytingAlgebra` instance for records.
class HeytingAlgebraRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class HeytingAlgebraRecord rowlist row subrow | rowlist -> subrow where
  ffRecord :: H1Record rowlist row subrow
  ttRecord :: H1Record rowlist row subrow
  impliesRecord :: H2Record rowlist row -> Record row -> Record subrow
  disjRecord :: H2Record rowlist row -> Record row -> Record subrow
  conjRecord :: H2Record rowlist row -> Record row -> Record subrow
  notRecord :: H2Record rowlist row -> Record subrow

instance heytingAlgebraRecordNil :: HeytingAlgebraRecord RL.Nil row () where
  conjRecord _ _ = {}
  disjRecord _ _ = {}
  ffRecord = H1Record {}
  impliesRecord _ _ = {}
  notRecord _ = {}
  ttRecord = H1Record {}

instance heytingAlgebraRecordCons ::
  ( IsSymbol key
  , Row.Cons key focus subrowTail subrow
  , HeytingAlgebraRecord rowlistTail row subrowTail
  , HeytingAlgebra focus
  ) =>
  HeytingAlgebraRecord (RL.Cons key focus rowlistTail) row subrow where
  conjRecord (H2Record ra) rb = insert (conj (get ra) (get rb)) tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = conjRecord (H2Record ra :: H2Record rowlistTail row) rb

  disjRecord (H2Record ra) rb = insert (disj (get ra) (get rb)) tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = disjRecord (H2Record ra :: H2Record rowlistTail row) rb

  impliesRecord (H2Record ra) rb = insert (implies (get ra) (get rb)) tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = impliesRecord (H2Record ra :: H2Record rowlistTail row) rb

  ffRecord = H1Record (insert ff tail)
    where
    key = reflectSymbol (Proxy :: Proxy key)
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = unH1Record (ffRecord :: H1Record rowlistTail row subrowTail)

  notRecord (H2Record r) = insert (not (get r)) tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = notRecord (H2Record r :: H2Record rowlistTail row)

  ttRecord = H1Record (insert tt tail)
    where
    key = reflectSymbol (Proxy :: Proxy key)
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = unH1Record (ttRecord :: H1Record rowlistTail row subrowTail)
