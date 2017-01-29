module Control.Applicative.Laws where

import Control.Applicative
import Control.Category (id, (<<<))
import Data.Function (($))
import Data.Eq (class Eq, (==))

-- | Checks the identity law for a given value:
-- |
-- |     (pure id) <*> v = v
identity :: forall f a. (Applicative f, Eq (f a)) => f a -> Boolean
identity v =
  (pure id) <*> v == v

-- | Checks the composition law for a given triple of values:
-- |
-- |     pure (<<<) <*> f <*> g <*> h = f <*> (g <*> h)
composition :: forall f a b c. (Applicative f, Eq (f c)) => f (b -> c) -> f (a -> b) -> f a -> Boolean
composition f g h =
  pure (<<<) <*> f <*> g <*> h == f <*> (g <*> h)

-- | Checks the homomorphism law for a given pair of values:
-- |
-- |     pure f <*> pure x = pure (f x)
homomorphism :: forall f a b. (Applicative f, Eq (f b)) => (a -> b) -> a -> Boolean
homomorphism f x =
  pure f <*> pure x == (pure (f x) :: f b)

-- | Checks the interchange law for a given pair of values:
-- |
-- |     u <*> pure y = pure (_ $ y) <*> u
interchange :: forall f a b. (Applicative f, Eq (f b)) => a -> f (a -> b) -> Boolean
interchange y u =
  u <*> pure y == pure (_ $ y) <*> u
