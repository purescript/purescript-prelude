module Data.Functor.Laws where

import Prelude hiding (identity)

import Control.Category as C

-- | `map identity = identity`
identity ∷ ∀ f a. Functor f ⇒ Eq (f a) ⇒ f a → Boolean
identity f = (map C.identity f) == C.identity f

-- | `map (f <<< g) = map f <<< map g`
composition ∷ ∀ f a b. Functor f ⇒ Eq (f a) ⇒ (b → a) → (a → b) → f a → Boolean
composition f g x = (map (f <<< g) x) == (((map f) <<< (map g)) x)
