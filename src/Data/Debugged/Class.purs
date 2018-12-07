-- | This module provides the `Debug` type class, for converting values into
-- | their `Debugged` representations.
module Data.Debugged.Class where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Array as Array
import Data.Bifunctor (bimap)
import Record (get, delete)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Prelude (class RowToList)
import Prim.Row as Row
import Type.Row (kind RowList, Nil, Cons, RLProxy(..))
import Effect (Effect)

import Data.Debugged.Type as D

-- | Ideally, all types of kind `Type` should have an instance of this class.
-- | If you are defining a type where it's difficult/impossible to do anything
-- | useful here (e.g. `Ref` or `(->)`) then you should use the `DOpaque`
-- | constructor.
-- |
-- | If a type has an `Eq` instance, then the `debugged` function in its `Debug`
-- | instance should be *injective*, that is:
-- |
-- | ```purescript
-- | x /= y `implies` debugged x /= debugged y
-- | ```
class Debug a where
  debugged :: a -> D.Repr

-- Prim
instance debugInt :: Debug Int where
  debugged = D.int

instance debugNumber :: Debug Number where
  debugged = D.number

instance debugBoolean :: Debug Boolean where
  debugged = D.boolean

instance debugString :: Debug String where
  debugged = D.string

instance debugChar :: Debug Char where
  debugged = D.char

instance debugArray :: Debug a => Debug (Array a) where
  debugged = D.array <<< map debugged

instance debugFunction :: Debug (a -> b) where
  debugged _ = D.opaque "function" []

class DebugRowList (list :: RowList) (row :: # Type) | list -> row where
  debugRowList :: RLProxy list -> Record row -> List (Tuple String D.Repr)

instance debugRowListNil :: DebugRowList Nil () where
  debugRowList _ _ = Nil

instance debugRowListCons ::
  ( Debug a
  , DebugRowList listRest rowRest
  , Row.Cons  key a rowRest rowFull
  , Row.Lacks key rowRest
  , RowToList rowFull (Cons key a listRest)
  , IsSymbol key
  ) => DebugRowList (Cons key a listRest) rowFull where
  debugRowList _ rec =
    Tuple (reflectSymbol key) (debugged val) : rest
    where
    key = SProxy :: SProxy key
    val = get key rec
    rest = debugRowList (RLProxy :: RLProxy listRest) (delete key rec)

instance debugRecord ::
  ( RowToList row list
  , DebugRowList list row
  ) => Debug (Record row) where
  debugged r =
    D.record (Array.fromFoldable (debugRowList prx r))
    where
    prx = RLProxy :: RLProxy list

-- Prelude
instance debugOrdering :: Debug Ordering where
  debugged LT = D.constructor "LT" []
  debugged EQ = D.constructor "EQ" []
  debugged GT = D.constructor "GT" []

instance debugUnit :: Debug Unit where
  debugged _ = D.constructor "unit" []

instance debugVoid :: Debug Void where
  debugged = absurd

-- Other

instance debugMaybe :: Debug a => Debug (Maybe a) where
  debugged (Just x) = D.constructor "Just" [debugged x]
  debugged Nothing = D.constructor "Nothing" []

instance debugEither :: (Debug a, Debug b) => Debug (Either a b) where
  debugged (Right x) = D.constructor "Right" [debugged x]
  debugged (Left x) = D.constructor "Left" [debugged x]

instance debugTuple :: (Debug a, Debug b) => Debug (Tuple a b) where
  debugged (Tuple x y) = D.constructor "Tuple" [debugged x, debugged y]

instance debugMap :: (Debug k, Debug v) => Debug (Map k v) where
  debugged m =
    D.assoc "Map"
      (map (bimap debugged debugged) (Map.toUnfoldable m))

instance debugEffect :: Debug (Effect a) where
  debugged _ = D.opaque "Effect" []

instance debugList :: Debug a => Debug (List a) where
  debugged xs = D.collection "List" (map debugged (List.toUnfoldable xs))

instance debugLazyList :: Debug a => Debug (LazyList.List a) where
  debugged xs = D.collection "List.Lazy" (map debugged (LazyList.toUnfoldable xs))

instance debugSet :: Debug a => Debug (Set a) where
  debugged s = D.collection "Set" (map debugged (Set.toUnfoldable s))

instance debugRepr :: Debug D.Repr where
  debugged r = D.opaque "Repr" [Tuple "value" r]

-- instance debugDebugged :: Debug Debugged where
--   debugged (DInt x) = DExpr "DInt" [debugged x]
--   debugged (DNumber x) = DExpr "DNumber" [debugged x]
--   debugged (DBoolean x) = DExpr "DBoolean" [debugged x]
--   debugged (DChar x) = DExpr "DChar" [debugged x]
--   debugged (DString x) = DExpr "DString" [debugged x]
--   debugged (DExpr name args) = DExpr "DExpr" [debugged name, debugged args]
--   debugged (DArray xs) = DExpr "DArray" [debugged xs]
--   debugged (DRecord xs) = DExpr "DRecord" [debugged xs]
--   debugged (DOpaque name xs) = DExpr "DOpaque" [debugged name, debugged xs]
--   debugged (DCollection name args) = DExpr "DCollection" [debugged name, debugged args]
--   debugged (DAssoc name args) = DExpr "DAssoc" [debugged name, debugged args]
