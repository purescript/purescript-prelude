-- | This module provides the `Debugged` type, which may be used as a common
-- | format for representations of values. The `Debugged` type is intended for
-- | use in testing, debugging, and in the REPL, but not in production code.
module Data.Debugged.Type where

import Prelude
import Data.Tuple (Tuple)

-- | A value of type `Debugged` is a representation of some PureScript value.
-- | It is often possible to reconstruct the original value from its `Debugged`
-- | representation, but not always. Notable counterexamples are `Eff`, `Ref`,
-- | and `(->)`.
data Debugged
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
