module Data.CommutativeRing.Laws where

import Prelude

-- | `a * b = b * a`
commutativeMul :: forall a. Eq a => CommutativeRing a => a -> a -> Boolean
commutativeMul a b = a * b == b * a
