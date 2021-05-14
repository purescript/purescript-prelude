module Data.Ord.Laws where

import Prelude

-- | `a <= a`
reflexivity ∷ ∀ a. Ord a ⇒ a → Boolean
reflexivity a = a <= a

-- | if `a <= b` and `b <= a` then `a = b`
antisymmetry ∷ ∀ a. Ord a ⇒ a → a → Boolean
antisymmetry a b = if (a <= b) && (b <= a) then a == b else a /= b

-- | if `a <= b` and `b <= c` then `a <= c`
transitivity ∷ ∀ a. Ord a ⇒ a → a → a → Boolean
transitivity a b c = if (a <= b) && (b <= c) then a <= c else true
