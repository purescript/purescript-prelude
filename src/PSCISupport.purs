-- | This module provides support for the
-- | PureScript interactive mode, PSCI.

module PSCI.Support where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Debugged (class Debug, debugged, prettyPrint)

-- | The `Eval` class captures those types which can be
-- | evaluated in the REPL.
-- |
-- | There are instances provided for the `Effect` type
-- | constructor and any `Debug` types.
class Eval a where
  eval :: a -> Effect Unit

instance evalEffect :: Eval a => Eval (Effect a) where
  eval x = x >>= eval
else
instance evalDebug :: Debug a => Eval a where
  eval = log <<< prettyPrint <<< debugged
