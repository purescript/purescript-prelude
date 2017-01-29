module Control.Bind.Laws where

import Control.Bind (class Bind, (>>=))
import Data.Eq (class Eq, (==))

-- | Checks the associativity law for a given triple of values:
-- |
-- |      (x >>= f) >>= g = x >>= (\k -> f k >>= g)
associativity :: forall m a b c. (Bind m, Eq (m c)) => m a -> (a -> m b) -> (b -> m c) -> Boolean
associativity x f g =
  ((x >>= f) >>= g) == (x >>= (\k -> f k >>= g))

