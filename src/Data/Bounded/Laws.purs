module Data.Bounded.Laws where

import Prelude

-- | `bottom <= a <= top`
ordering ∷ ∀ a. Bounded a ⇒ a → Boolean
ordering a = bottom <= a && a <= top
