-- | This module provides the `Debug` type class, for converting values into
-- | their `Debug` representations.
module Data.Debug.Class where

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

import Data.Debug.Type as D

-- | Ideally, all types of kind `Type` should have an instance of this class.
-- | If you are defining a type where it's difficult/impossible to do anything
-- | useful here (e.g. `Ref` or `(->)`) then you should use the `DOpaque`
-- | constructor.
-- |
-- | If a type has an `Eq` instance, then the `debug` function in its `Debug`
-- | instance should be *injective*, that is:
-- |
-- | ```purescript
-- | x /= y `implies` debug x /= debug y
-- | ```
class Debug a where
  debug :: a -> D.Repr

diffed :: forall a. Debug a => a -> a -> D.ReprDelta
diffed x y = D.diff (debug x) (debug y)

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
  debug _ = D.opaque "function" []

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
    Tuple (reflectSymbol key) (debug val) : rest
    where
    key = SProxy :: SProxy key
    val = get key rec
    rest = debugRowList (RLProxy :: RLProxy listRest) (delete key rec)

instance debugRecord ::
  ( RowToList row list
  , DebugRowList list row
  ) => Debug (Record row) where
  debug r =
    D.record (Array.fromFoldable (debugRowList prx r))
    where
    prx = RLProxy :: RLProxy list

-- Prelude
instance debugOrdering :: Debug Ordering where
  debug LT = D.constructor "LT" []
  debug EQ = D.constructor "EQ" []
  debug GT = D.constructor "GT" []

instance debugUnit :: Debug Unit where
  debug _ = D.constructor "unit" []

instance debugVoid :: Debug Void where
  debug = absurd

-- Other

instance debugMaybe :: Debug a => Debug (Maybe a) where
  debug (Just x) = D.constructor "Just" [debug x]
  debug Nothing = D.constructor "Nothing" []

instance debugEither :: (Debug a, Debug b) => Debug (Either a b) where
  debug (Right x) = D.constructor "Right" [debug x]
  debug (Left x) = D.constructor "Left" [debug x]

instance debugTuple :: (Debug a, Debug b) => Debug (Tuple a b) where
  debug (Tuple x y) = D.constructor "Tuple" [debug x, debug y]

instance debugMap :: (Debug k, Debug v) => Debug (Map k v) where
  debug m =
    D.assoc "Map"
      (map (bimap debug debug) (Map.toUnfoldable m))

instance debugEffect :: Debug (Effect a) where
  debug _ = D.opaque "Effect" []

instance debugList :: Debug a => Debug (List a) where
  debug xs = D.collection "List" (map debug (List.toUnfoldable xs))

instance debugLazyList :: Debug a => Debug (LazyList.List a) where
  debug xs = D.collection "List.Lazy" (map debug (LazyList.toUnfoldable xs))

instance debugSet :: Debug a => Debug (Set a) where
  debug s = D.collection "Set" (map debug (Set.toUnfoldable s))

instance debugRepr :: Debug D.Repr where
  debug r = D.opaque "Repr" [Tuple "value" r]
