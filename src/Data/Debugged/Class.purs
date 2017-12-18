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
import Data.Record (get, delete)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Prelude (class RowToList, class RowLacks)
import Type.Row (kind RowList, Nil, Cons, RLProxy(..))
import Control.Monad.Eff (Eff)

import Data.Debugged.Type (Debugged(..))

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
  debugged :: a -> Debugged

-- Prim
instance debugInt :: Debug Int where
  debugged = DInt

instance debugNumber :: Debug Number where
  debugged = DNumber

instance debugBoolean :: Debug Boolean where
  debugged = DBoolean

instance debugString :: Debug String where
  debugged = DString

instance debugChar :: Debug Char where
  debugged = DChar

instance debugArray :: Debug a => Debug (Array a) where
  debugged = DArray <<< map debugged

instance debugFunction :: Debug (a -> b) where
  debugged _ = DOpaque "function" []

class DebugRowList (list :: RowList) (row :: # Type) | list -> row where
  debugRowList :: RLProxy list -> Record row -> List (Tuple String Debugged)

instance debugRowListNil :: DebugRowList Nil () where
  debugRowList _ _ = Nil

instance debugRowListCons ::
  ( Debug a
  , DebugRowList listRest rowRest
  , RowCons  key a rowRest rowFull
  , RowLacks key rowRest
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
    DRecord (Array.fromFoldable (debugRowList prx r))
    where
    prx = RLProxy :: RLProxy list

-- Prelude
instance debugOrdering :: Debug Ordering where
  debugged LT = DExpr "LT" []
  debugged EQ = DExpr "EQ" []
  debugged GT = DExpr "GT" []

instance debugUnit :: Debug Unit where
  debugged _ = DExpr "unit" []

instance debugVoid :: Debug Void where
  debugged = absurd

-- Other

instance debugMaybe :: Debug a => Debug (Maybe a) where
  debugged (Just x) = DExpr "Just" [debugged x]
  debugged Nothing = DExpr "Nothing" []

instance debugEither :: (Debug a, Debug b) => Debug (Either a b) where
  debugged (Right x) = DExpr "Right" [debugged x]
  debugged (Left x) = DExpr "Left" [debugged x]

instance debugTuple :: (Debug a, Debug b) => Debug (Tuple a b) where
  debugged (Tuple x y) = DExpr "Tuple" [debugged x, debugged y]

instance debugMap :: (Debug k, Debug v) => Debug (Map k v) where
  debugged m =
    DAssoc "Map"
      (map (bimap debugged debugged) (Map.toUnfoldable m))

instance debugEff :: Debug (Eff eff a) where
  debugged _ = DOpaque "Eff" []

instance debugList :: Debug a => Debug (List a) where
  debugged xs = DCollection "List" (map debugged (List.toUnfoldable xs))

instance debugLazyList :: Debug a => Debug (LazyList.List a) where
  debugged xs = DCollection "List.Lazy" (map debugged (LazyList.toUnfoldable xs))

instance debugSet :: Debug a => Debug (Set a) where
  debugged s = DCollection "Set" (map debugged (Set.toUnfoldable s))

instance debugDebugged :: Debug Debugged where
  debugged (DInt x) = DExpr "DInt" [debugged x]
  debugged (DNumber x) = DExpr "DNumber" [debugged x]
  debugged (DBoolean x) = DExpr "DBoolean" [debugged x]
  debugged (DChar x) = DExpr "DChar" [debugged x]
  debugged (DString x) = DExpr "DString" [debugged x]
  debugged (DExpr name args) = DExpr "DExpr" [debugged name, debugged args]
  debugged (DArray xs) = DExpr "DArray" [debugged xs]
  debugged (DRecord xs) = DExpr "DRecord" [debugged xs]
  debugged (DOpaque name xs) = DExpr "DOpaque" [debugged name, debugged xs]
  debugged (DCollection name args) = DExpr "DCollection" [debugged name, debugged args]
  debugged (DAssoc name args) = DExpr "DAssoc" [debugged name, debugged args]
