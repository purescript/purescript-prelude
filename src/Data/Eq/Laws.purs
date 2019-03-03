module Data.Eq.Laws where

import Prelude

-- | `x == x = true`
reflexivity ∷ ∀ a. Eq a ⇒ a → Boolean
reflexivity x = (x == x) == true

-- | `x == y = y == x`
symmetry ∷ ∀ a. Eq a ⇒ a → a → Boolean
symmetry x y = (x == y) == (y == x)

-- | if `x == y` and `y == z` then `x == z`
transitivity ∷ ∀ a. Eq a ⇒ a → a → a → Boolean
transitivity x y z = if (x == y) && (y == z) then x == z else true
