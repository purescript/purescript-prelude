module Data.DivisionRing.Laws where

import Prelude

-- | `one /= zero`
divisionRingIdentity :: forall proxy a. Eq a => DivisionRing a => proxy a -> Boolean
divisionRingIdentity _ = one /= (zero :: a)

-- | For all nonzero a `recip a * a = a * recip a = one`
multiplicativeInverse :: forall a. Eq a => DivisionRing a => a -> a -> Boolean
multiplicativeInverse a b
  | a /= zero = (recip a * a == one) && (a * recip a == one)
  | otherwise = true
