module Data.Debugged where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Foldable (all)
import Data.Record (get, delete)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Prelude (class RowToList, class RowLacks)
import Type.Row (kind RowList, Nil, Cons, RLProxy(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- Converts a data type to a debugging representation; from here, it can be
-- e.g. printed in a REPL, or diffed with an expected representation for
-- display in a failing test.
data Debugged
  -- Constructors which can be 'uneval'ed
  = DInt Int
  | DNumber Number
  | DBoolean Boolean
  | DChar Char
  | DString String
  | DExpr String (Array Debugged)
  | DArray (Array Debugged)
  | DRecord (Array (Tuple String Debugged))

  -- These constructors are for representations of opaque data types, or data
  -- types where this representation is more helpful than the 'obvious'
  -- representation (e.g. List).
  | DOpaque String (Array (Tuple String Debugged))
  | DCollection String (Array Debugged)
  | DAssoc String (Array (Tuple Debugged Debugged))

derive instance eqDebugged :: Eq Debugged
derive instance ordDebugged :: Ord Debugged

-- Ideally, all types of kind `Type` should have an instance of this class. If
-- you are defining a type where it's difficult/impossible to do anything
-- useful here (e.g. Ref or (->)) then you should just write something like
-- DAtom "<Ref>".
--
-- If a type has an `Eq` instance, then the `debugged` function in its `Debug`
-- instance should be *injective*, that is:
--
-- ```purescript
-- x /= y `implies` debugged x /= debugged y
-- ```
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

---- Pretty-printing

indent :: String -> String
indent = ("  " <> _)

mapAllButLast :: forall a. (a -> a) -> Array a -> Array a
mapAllButLast f xs =
  let
    len = Array.length xs
  in
    Array.modifyAtIndices (Array.range 0 (len-2)) f xs

withLast :: forall a. (a -> a) -> Array a -> Array a
withLast f xs =
  let
    len = Array.length xs
  in
    fromMaybe xs (Array.modifyAt (len - 1) f xs)

-- pretty print a `Debugged` value, given a maximum recursion depth. Returns
-- an array of lines.
-- prettyPrint :: Int -> Debugged -> Array String
-- prettyPrint _ (DInt x) = [show x]
-- prettyPrint _ (DNumber x) = [show x]
-- prettyPrint _ (DBoolean x) = [show x]
-- prettyPrint _ (DAtom x) = [x]
-- prettyPrint _ (DArray []) = ["[]"]
-- prettyPrint depth (DArray xs) =
--   if depth <= 0
--     then ["[...]"]
--     else ["["]
--           <> (xs >>= (map indent <<< (prettyPrint (depth - 1))))
--           <> ["]"]
-- prettyPrint _ (DRecord []) = ["{}"]
-- prettyPrint depth (DRecord xs) =
--   if depth <= 0
--     then
--       ["{...}"]
--     else
--       ["{"]
--        <> (xs >>= \(Tuple key value) ->
--             case prettyPrint (depth - 1) value of
--               [v] -> [indent (key <> ": " <> v)]
--               vs -> [key <> ":"] <> map indent vs
--           )
--        <> ["}"]
-- prettyPrint depth (DCtor name args) =
--   if depth <= 0
--     then
--       if Array.length args >= 1
--         then [name <> " <...>"]
--         else [name]
--     else
--       let
--         args' :: Array String
--         args' = args >>= prettyPrint (depth - 1)
--       in
--         if all isAtomic args
--           then [String.joinWith " " ([name] <> map String.trim args')]
--           else [name] <> map indent args'
-- prettyPrint depth (DCollection _ xs) =
--   -- TODO
--   prettyPrint depth (DArray xs)
-- prettyPrint depth (DAssoc name xs) =
--   if depth <= 0
--     then
--       [name <> " <...>"]
--     else
--       let
--         go = prettyPrint (depth - 1)
--       in
--         [name]
--          <> (xs >>= \(Tuple key value) ->
--               case go key, go value of
--                 [k], [v] -> [indent (k <> ": " <> v)]
--                 [k], vs -> [k <> ":"] <> map indent vs
--                 _, _ -> ["..."]
--             )

prettyPrintOneLine :: Debugged -> String
prettyPrintOneLine =
  case _ of
    DInt x ->
      show x
    DNumber x ->
      show x
    DBoolean x ->
      show x
    DChar x ->
      show x
    DString x ->
      show x
    DExpr name [] ->
      name
    DExpr name args ->
      name <> " " <> String.joinWith " " (map prettyPrintAtom args)
    DArray xs ->
      "[" <>
        String.joinWith ", " (map prettyPrintOneLine xs) <>
        "]"
    DRecord xs ->
      "{" <>
        String.joinWith ", " (map printRecord xs) <>
        "}"
    DOpaque name [] ->
      "<" <> name <> ">"
    DOpaque name xs ->
      "<" <> name <> " " <>
        String.joinWith ", " (map printRecord xs)
        <> ">"
    DCollection name args ->
      "<" <> name <> " [" <>
        String.joinWith ", " (map prettyPrintOneLine args) <>
        "]>"
    DAssoc name args ->
      "<" <> name <> " {" <>
        String.joinWith ", " (map printAssoc args) <>
        "}>"
  where
  printRecord (Tuple k v) = k <> ": " <> prettyPrintOneLine v
  printAssoc (Tuple a b) = prettyPrintAtom a <> ": " <> prettyPrintOneLine b

-- Pretty print a value on one line, adding parentheses if necessary.
needsParens :: Debugged -> Boolean
needsParens =
  case _ of
    DInt x     -> x < 0
    DNumber x  -> x < 0.0 -- TODO: negative zero?
    DExpr _ [] -> false
    DExpr _ _  -> true
    _          -> false

prettyPrintAtom :: Debugged -> String
prettyPrintAtom d =
  if needsParens d
    then "(" <> prettyPrintOneLine d <> ")"
    else prettyPrintOneLine d

-- print :: forall eff a. Debug a => a -> Eff (console :: CONSOLE | eff) Unit
-- print = log <<< String.joinWith "\n" <<< prettyPrint top <<< debugged

print' :: forall eff a. Debug a => a -> Eff (console :: CONSOLE | eff) Unit
print' = log <<< prettyPrintOneLine <<< debugged

eval :: forall eff a. Debug a => a -> Eff (console :: CONSOLE | eff) Unit
eval = print'
