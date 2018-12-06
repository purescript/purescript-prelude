module PSCI.Support ( eval ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Debugged (class Debug, debugged, prettyPrintOneLine)

eval :: forall a. Debug a => a -> Effect Unit
eval = log <<< prettyPrintOneLine <<< debugged
