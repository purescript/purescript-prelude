module Data.Monoid.Laws where

import Prelude

import Laws.Algebraic as Laws

-- | `(mempty <> x) = x`
leftIdentity ∷ ∀ a. Monoid a ⇒ Eq a ⇒ a → Boolean
leftIdentity = Laws.leftIdentity (<>) mempty

-- | `(x <> mempty) = x`
rightIdentity ∷ ∀ a. Monoid a ⇒ Eq a ⇒ a → Boolean
rightIdentity = Laws.rightIdentity (<>) mempty
