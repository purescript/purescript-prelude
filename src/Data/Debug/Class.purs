-- | This module provides the `Debug` type class, for converting values into
-- | their `Debug` representations.
module Data.Debug.Class
  ( class Debug
  , debug
  , class DebugRowList
  , debugRowList
  ) where

import Prelude

import Data.Debug.Type as D
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList, Nil, Cons)
import Record.Unsafe (unsafeDelete, unsafeGet)
import Type.Proxy (Proxy(..))

-- | Ideally, all types of kind `Type` should have an instance of this class.
-- | If you are defining a type where it's difficult/impossible to do anything
-- | useful here (e.g. `Ref` or `(->)`) then you should use the `opaque`
-- | constructor.
-- |
-- | If a type has an `Eq` instance, then the `debug` function in its `Debug`
-- | instance should be *injective*, that is:
-- |
-- | ```purescript
-- | x /= y `implies` debug x /= debug y
-- | ```
class Debug :: Type -> Constraint
class Debug a where
  debug :: a -> D.Repr

-------------------------------------------------------------------------------
-- Prim

instance debugInt :: Debug Int where
  debug = D.int

instance debugNumber :: Debug Number where
  debug = D.number

instance debugBoolean :: Debug Boolean where
  debug = D.boolean

instance debugString :: Debug String where
  debug = D.string

instance debugChar :: Debug Char where
  debug = D.char

instance debugArray :: Debug a => Debug (Array a) where
  debug = D.array <<< map debug

instance debugFunction :: Debug (a -> b) where
  debug _ = D.opaque_ "function"

-- | This class is part of the machinery for the `Debug (Record r)` instance;
-- | it is not intended to be used directly.
class DebugRowList :: RowList Type -> Row Type -> Constraint
class DebugRowList list row | list -> row where
  debugRowList :: Proxy list -> Record row -> Array { key :: String, value :: D.Repr }

instance debugRowListNil :: DebugRowList Nil () where
  debugRowList _ _ = []

instance debugRowListCons ::
  ( Debug a
  , DebugRowList listRest rowRest
  , Row.Cons  key a rowRest rowFull
  , Row.Lacks key rowRest
  , RowToList rowFull (Cons key a listRest)
  , IsSymbol key
  ) => DebugRowList (Cons key a listRest) rowFull where
  debugRowList _ rec =
    cons { key, value: debug val } rest
    where
    key = reflectSymbol (Proxy :: Proxy key)
    val = unsafeGet key rec :: a
    rest = debugRowList (Proxy :: Proxy listRest) (unsafeDelete key rec)

instance debugRecord ::
  ( RowToList row list
  , DebugRowList list row
  ) => Debug (Record row) where
  debug r =
    D.record (debugRowList prx r)
    where
    prx = Proxy :: Proxy list

-------------------------------------------------------------------------------
-- Prelude

instance debugOrdering :: Debug Ordering where
  debug LT = D.constructor "LT" []
  debug EQ = D.constructor "EQ" []
  debug GT = D.constructor "GT" []

instance debugUnit :: Debug Unit where
  debug _ = D.constructor "unit" []

instance debugVoid :: Debug Void where
  debug = absurd

foreign import cons :: forall a. a -> Array a -> Array a