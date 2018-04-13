module Data.Semigroup.Max where

import Prelude

import Data.Eq (class Eq1)
import Data.Ord (class Ord1)

-- | Semigroup where `append` always takes the greater value.
-- |
-- | ``` purescript
-- | Max x <> Max y == Max (if x >= y then x else y)
-- | ```
newtype Max a = Max a

derive newtype instance eqMax :: Eq a => Eq (Max a)
derive instance eq1Max :: Eq1 Max

derive newtype instance ordMax :: Ord a => Ord (Max a)
derive instance ord1Max :: Ord1 Max

derive newtype instance boundedMax :: Bounded a => Bounded (Max a)

instance showMax :: Show a => Show (Max a) where
  show (Max a) = "(Max " <> show a <> ")"

derive instance functorMax :: Functor Max

instance applyMax :: Apply Max where
  apply (Max f) (Max x) = Max (f x)

instance applicativeMax :: Applicative Max where
  pure = Max

instance bindMax :: Bind Max where
  bind (Max x) f = f x

instance monadMax :: Monad Max

instance semigroupMax :: Ord a => Semigroup (Max a) where
  append (Max x) (Max y) = Max (max x y)
