-- | This module provides functions for pretty-printing `Debugged`
-- | representations.
module Data.Debugged.Pretty where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.Array as Array
import Effect (Effect)
import Effect.Console (log)

import Data.Debugged.Type (Debugged(..))
import Data.Debugged.Class (class Debug, debugged)

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

-- | Pretty-print a representation on a single line.
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

-- | Check whether a representation needs to be wrapped in parens when being
-- | displayed in a context which would require them (e.g. as an argument to
-- | a data constructor).
needsParens :: Debugged -> Boolean
needsParens =
  case _ of
    DInt x     -> x < 0
    DNumber x  -> x < 0.0 -- TODO: negative zero?
    DExpr _ [] -> false
    DExpr _ _  -> true
    _          -> false

-- | Pretty-print a representation, adding parens if necessary.
prettyPrintAtom :: Debugged -> String
prettyPrintAtom d =
  if needsParens d
    then "(" <> prettyPrintOneLine d <> ")"
    else prettyPrintOneLine d

-- print :: forall eff a. Debug a => a -> Eff (console :: CONSOLE | eff) Unit
-- print = log <<< String.joinWith "\n" <<< prettyPrint top <<< debugged

print' :: forall eff a. Debug a => a -> Effect Unit
print' = log <<< prettyPrintOneLine <<< debugged

eval :: forall eff a. Debug a => a -> Effect Unit
eval = print'
