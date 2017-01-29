module Control.Apply.Laws where

import Control.Apply (class Apply, (<*>), (<$>))
import Control.Category ((<<<))
import Data.Eq (class Eq, (==))

-- | Checks the associative composition law for a given triple of values:
-- |
-- |     (<<<) <$> f <*> g <*> h = f <*> (g <*> h)
associativeComposition :: forall f a b c. (Apply f, Eq (f c)) => f (b -> c) -> f (a -> b) -> f a -> Boolean
associativeComposition f g h =
  (<<<) <$> f <*> g <*> h == f <*> (g <*> h)
