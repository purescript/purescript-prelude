module Data.Monoid.Equiv
  ( Equiv(..)
  ) where

import Prelude

import Data.Eq (class Eq1)
import Data.HeytingAlgebra (implies, tt)
import Data.Ord (class Ord1)

-- | Monoid and semigroup for equivalence.
-- |
-- | ```purescript
-- | Equiv x <> Equiv y = Equiv ((x `implies` y) && (y `implies` x))
-- | mempty :: Equiv _ = Equiv tt
-- | ```
newtype Equiv a = Equiv a

equiv :: forall a. HeytingAlgebra a => a -> a -> a
equiv x y = (x `implies` y) && (y `implies` x)

derive newtype instance eqEquiv :: Eq a => Eq (Equiv a)
derive instance eq1Equiv :: Eq1 Equiv

derive newtype instance ordEquiv :: Ord a => Ord (Equiv a)
derive instance ord1Equiv :: Ord1 Equiv

derive newtype instance boundedEquiv :: Bounded a => Bounded (Equiv a)

instance showEquiv :: Show a => Show (Equiv a) where
  show (Equiv a) = "(Equiv " <> show a <> ")"

derive instance functorEquiv :: Functor Equiv

instance applyEquiv :: Apply Equiv where
  apply (Equiv f) (Equiv x) = Equiv (f x)

instance applicativeEquiv :: Applicative Equiv where
  pure = Equiv

instance bindEquiv :: Bind Equiv where
  bind (Equiv a) k = k a

instance monadEquiv :: Monad Equiv

instance semigroupEquiv :: HeytingAlgebra a => Semigroup (Equiv a) where
  append (Equiv a) (Equiv b) = Equiv (a `equiv` b)

instance monoidEquiv :: HeytingAlgebra a => Monoid (Equiv a) where
  mempty = Equiv tt
