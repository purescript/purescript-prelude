module Data.Semigroup.Min where

import Prelude

import Data.Eq (class Eq1)
import Data.Ord (class Ord1)

-- | Semigroup where `append` always takes the lesser value.
-- |
-- | ``` purescript
-- | Min x <> Min y == Min (if x <= y then x else y)
-- | ```
newtype Min a = Min a

derive newtype instance eqMin :: Eq a => Eq (Min a)
derive instance eq1Min :: Eq1 Min

derive newtype instance ordMin :: Ord a => Ord (Min a)
derive instance ord1Min :: Ord1 Min

derive newtype instance boundedMin :: Bounded a => Bounded (Min a)

instance showMin :: Show a => Show (Min a) where
  show (Min a) = "(Min " <> show a <> ")"

derive instance functorMin :: Functor Min

instance applyMin :: Apply Min where
  apply (Min f) (Min x) = Min (f x)

instance applicativeMin :: Applicative Min where
  pure = Min

instance bindMin :: Bind Min where
  bind (Min x) f = f x

instance monadMin :: Monad Min

instance semigroupMin :: Ord a => Semigroup (Min a) where
  append (Min x) (Min y) = Min (min x y)
