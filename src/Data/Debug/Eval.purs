-- | This module provides support for using `Debug` to print values in the
-- | PureScript repl.
-- |
-- | To make use of this, simply issue
-- |
-- | ```
-- | > :print Data.Debug.Eval.eval
-- | ```
-- |
-- | in a repl session.
module Data.Debug.Eval where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Debug (class Debug, debug)
import Data.Debug.PrettyPrinter (prettyPrint)

-- | The `Eval` class captures those types which can be evaluated in the REPL.
-- |
-- | There are instances provided for the `Effect` type constructor and any
-- | `Debug` types.
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
