module Data.Semigroup.Laws where

import Prelude

import Laws.Algebraic as Laws

-- | ((x <> y) <> z) = (x <> (y <> z))
associative ∷ ∀ a. Semigroup a ⇒ Eq a ⇒ a → a → a → Boolean
associative = Laws.associative (<>)
