-- | This module provides support for the PureScript interactive mode, PSCI.
-- |
-- | This module is a bit of a hack. See the README for further details.

module PSCI.Support where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Debug (class Debug, debug, prettyPrint)

-- | The `Eval` class captures those types which can be
-- | evaluated in the REPL.
-- |
-- | There are instances provided for the `Effect` type
-- | constructor and any `Debug` types.
class Eval a where
  eval :: a -> Effect Unit

instance evalEffectUnit :: Eval (Effect Unit) where
  eval = identity
else
instance evalEffect :: Eval a => Eval (Effect a) where
  eval x = x >>= eval
else
instance evalDebug :: Debug a => Eval a where
  eval = log <<< prettyPrint <<< debug
