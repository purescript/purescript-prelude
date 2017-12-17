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

-- The "DAtom" constructor is intended only for the simplest values. It's also
-- the preferred option for when types cannot provide any information about
-- themselves (e.g. Ref or (->)).
data Debugged
  = DInt Int
  | DNumber Number
  | DBoolean Boolean
  | DAtom String
  | DCtor String (Array Debugged)
  | DArray (Array Debugged)
  | DRecord (Array (Tuple String Debugged))

  -- These two constructors are for representations of opaque data types, or
  -- data types where this representation is more helpful than the 'obvious'
  -- representation (e.g. List).
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
  debugged = DAtom <<< show

instance debugChar :: Debug Char where
  debugged = DAtom <<< show

instance debugArray :: Debug a => Debug (Array a) where
  debugged = DArray <<< map debugged

instance debugFunction :: Debug (a -> b) where
  debugged _ = DAtom "<function>"

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
  debugged LT = DCtor "LT" []
  debugged EQ = DCtor "EQ" []
  debugged GT = DCtor "GT" []

instance debugUnit :: Debug Unit where
  debugged _ = DAtom "unit"

instance debugVoid :: Debug Void where
  debugged = absurd

-- Other

instance debugMaybe :: Debug a => Debug (Maybe a) where
  debugged (Just x) = DCtor "Just" [debugged x]
  debugged Nothing = DCtor "Nothing" []

instance debugEither :: (Debug a, Debug b) => Debug (Either a b) where
  debugged (Right x) = DCtor "Right" [debugged x]
  debugged (Left x) = DCtor "Left" [debugged x]

instance debugTuple :: (Debug a, Debug b) => Debug (Tuple a b) where
  debugged (Tuple x y) = DCtor "Tuple" [debugged x, debugged y]

instance debugMap :: (Debug k, Debug v) => Debug (Map k v) where
  debugged m =
    DAssoc "Map"
      (map (bimap debugged debugged) (Map.toUnfoldable m))

instance debugEff :: Debug (Eff eff a) where
  debugged _ = DAtom "<Eff>"

instance debugList :: Debug a => Debug (List a) where
  debugged xs = DCollection "List" (map debugged (List.toUnfoldable xs))

instance debugLazyList :: Debug a => Debug (LazyList.List a) where
  debugged xs = DCollection "List.Lazy" (map debugged (LazyList.toUnfoldable xs))

instance debugSet :: Debug a => Debug (Set a) where
  debugged s = DCollection "Set" (map debugged (Set.toUnfoldable s))

instance debugDebugged :: Debug Debugged where
  debugged (DInt x) = DCtor "DInt" [debugged x]
  debugged (DNumber x) = DCtor "DNumber" [debugged x]
  debugged (DBoolean x) = DCtor "DBoolean" [debugged x]
  debugged (DAtom x) = DCtor "DAtom" [debugged x]
  debugged (DCtor name args) = DCtor "DCtor" [debugged name, debugged args]
  debugged (DArray xs) = DCtor "DArray" [debugged xs]
  debugged (DRecord xs) = DCtor "DRecord" [debugged xs]
  debugged (DCollection name args) = DCtor "DCollection" [debugged name, debugged args]
  debugged (DAssoc name args) = DCtor "DAssoc" [debugged name, debugged args]

---- Pretty-printing

indent :: String -> String
indent = ("  " <> _)

-- To be considered "atomic" in this context, a piece of data must satisfy two
-- conditions:
-- * It must be sufficiently simple that it makes sense to condense it
--   onto a line together with some other pieces of atomic data.
-- * It must be able to be placed next to other pieces of atomic data without
--   needing parentheses. For example, data constructors with one or more
--   argument fail this criterion because e.g. condensing Just (Just (Just 3))
--   onto one line would yield "Just Just Just 3", which is not what we want.
isAtomic :: Debugged -> Boolean
isAtomic =
  case _ of
    DInt _ -> true
    DNumber _ -> true
    DBoolean _ -> true
    DAtom _ -> true

    DArray [] -> true
    DArray [x] -> isAtomic x
    DArray _ -> false

    DRecord _ -> false

    -- data constructors are only considered "atomic" if they have no arguments
    DCtor _ [] -> true
    DCtor _ _ -> false

    DCollection _ _ -> false

    DAssoc _ _ -> false

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
prettyPrint :: Int -> Debugged -> Array String
prettyPrint _ (DInt x) = [show x]
prettyPrint _ (DNumber x) = [show x]
prettyPrint _ (DBoolean x) = [show x]
prettyPrint _ (DAtom x) = [x]
prettyPrint _ (DArray []) = ["[]"]
prettyPrint depth (DArray xs) =
  if depth <= 0
    then ["[...]"]
    else ["["]
          <> (xs >>= (map indent <<< (prettyPrint (depth - 1))))
          <> ["]"]
prettyPrint _ (DRecord []) = ["{}"]
prettyPrint depth (DRecord xs) =
  if depth <= 0
    then
      ["{...}"]
    else
      ["{"]
       <> (xs >>= \(Tuple key value) ->
            case prettyPrint (depth - 1) value of
              [v] -> [indent (key <> ": " <> v)]
              vs -> [key <> ":"] <> map indent vs
          )
       <> ["}"]
prettyPrint depth (DCtor name args) =
  if depth <= 0
    then
      if Array.length args >= 1
        then [name <> " <...>"]
        else [name]
    else
      let
        args' :: Array String
        args' = args >>= prettyPrint (depth - 1)
      in
        if all isAtomic args
          then [String.joinWith " " ([name] <> map String.trim args')]
          else [name] <> map indent args'
prettyPrint depth (DCollection _ xs) =
  -- TODO
  prettyPrint depth (DArray xs)
prettyPrint depth (DAssoc name xs) =
  if depth <= 0
    then
      [name <> " <...>"]
    else
      let
        go = prettyPrint (depth - 1)
      in
        [name]
         <> (xs >>= \(Tuple key value) ->
              case go key, go value of
                [k], [v] -> [indent (k <> ": " <> v)]
                [k], vs -> [k <> ":"] <> map indent vs
                _, _ -> ["..."]
            )

prettyPrintOneLine :: Debugged -> String
prettyPrintOneLine =
  case _ of
    DInt x ->
      show x
    DNumber x ->
      show x
    DBoolean x ->
      show x
    DAtom x ->
      x
    DCtor name [] ->
      name
    DCtor name args ->
      name <> " " <> String.joinWith " " (map prettyPrintAtom args)
    DArray xs ->
      "[" <>
        String.joinWith ", " (map prettyPrintOneLine xs) <>
        "]"
    DRecord xs ->
      "{" <>
        String.joinWith ", " (map (\(Tuple k v) -> k <> ": " <> prettyPrintOneLine v) xs) <>
        "}"
    DCollection name args ->
      "<" <> name <> " [" <>
        String.joinWith ", " (map prettyPrintOneLine args) <>
        "]>"
    DAssoc name args ->
      "<" <> name <> " {" <>
        String.joinWith ", " (map printAssoc args) <>
        "}>"
  where
  printAssoc (Tuple a b) = prettyPrintAtom a <> ": " <> prettyPrintOneLine b

-- Pretty print a value on one line, adding parentheses if necessary.
prettyPrintAtom :: Debugged -> String
prettyPrintAtom =
  case _ of
    DInt x | x < 0 ->
      "(" <> show x <> ")"
    DNumber x | x < 0.0 ->
      "(" <> show x <> ")"
    DCtor name [] ->
      name
    DCtor name args ->
      "(" <> name <>
        " " <> String.joinWith " " (map prettyPrintAtom args) <>
        ")"
    other ->
      prettyPrintOneLine other

print :: forall eff a. Debug a => a -> Eff (console :: CONSOLE | eff) Unit
print = log <<< String.joinWith "\n" <<< prettyPrint top <<< debugged

print' :: forall eff a. Debug a => a -> Eff (console :: CONSOLE | eff) Unit
print' = log <<< prettyPrintOneLine <<< debugged

eval :: forall eff a. Debug a => a -> Eff (console :: CONSOLE | eff) Unit
eval = print
