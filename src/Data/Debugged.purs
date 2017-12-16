module Data.Debugged where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (all)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.String as String
import Data.Array as Array

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
  | DArray (Array Debugged)
  | DCtor String (Array Debugged)
  | DAssoc String (Array (Tuple String Debugged))

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

-- TODO: Records

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

-- TODO: Debug (Record a) instance

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

    -- data constructors are only considered "atomic" if they have no arguments
    DCtor _ [] -> true
    DCtor _ _ -> false

    DAssoc _ _ -> false

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
        if Array.length args <= 3 && all isAtomic args
          then [String.joinWith " " ([name] <> map String.trim args')]
          else [name] <> map indent args'
prettyPrint depth (DAssoc name xs) =
  if depth <= 0
    then
      [name <> " <...>"]
    else
      [name]
       <> (xs >>= \(Tuple key value) ->
            case prettyPrint (depth - 1) value of
              [v] -> [indent (key <> ": " <> v)]
              vs -> [key <> ":"] <> map indent vs
            )

print :: forall eff a. Debug a => a -> Eff (console :: CONSOLE | eff) Unit
print = log <<< String.joinWith "\n" <<< prettyPrint top <<< debugged
