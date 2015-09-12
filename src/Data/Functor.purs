module Data.Functor
  ( Functor
  , map
  , (<$>)
  , ($>)
  , (<$)
  , (<#>)
  , void
  ) where

import Data.Function (const)
import Data.Semigroupoid (compose)
import Data.Unit (Unit(), unit)

-- | A `Functor` is a type constructor which supports a mapping operation
-- | `(<$>)`.
-- |
-- | `(<$>)` can be used to turn functions `a -> b` into functions
-- | `f a -> f b` whose argument and return types use the type constructor `f`
-- | to represent some computational context.
-- |
-- | Instances must satisfy the following laws:
-- |
-- | - Identity: `(<$>) id = id`
-- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

instance functorFn :: Functor ((->) r) where
  map = compose

instance functorArray :: Functor Array where
  map = mapArrayImpl

infixl 4 <$>
infixl 4 <$
infixl 4 $>
infixl 1 <#>

-- | `(<$>)` is an alias for `map`
(<$>) :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
(<$>) = map

-- | Ignore the return value of a computation, using the specified return value instead.
(<$) :: forall f a b. (Functor f) => a -> f b -> f a
(<$) a f = const a <$> f

-- | A version of `(<$)` with its arguments flipped.
($>) :: forall f a b. (Functor f) => f a -> b -> f b
($>) f b = const b <$> f


-- | `(<#>)` is `(<$>)` with its arguments reversed. For example:
-- |
-- | ```purescript
-- | [1, 2, 3] <#> \n -> n * n
-- | ```
(<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
(<#>) fa f = f <$> fa

-- | The `void` function is used to ignore the type wrapped by a
-- | [`Functor`](#functor), replacing it with `Unit` and keeping only the type
-- | information provided by the type constructor itself.
-- |
-- | `void` is often useful when using `do` notation to change the return type
-- | of a monadic computation:
-- |
-- | ```purescript
-- | main = forE 1 10 \n -> void do
-- |   print n
-- |   print (n * n)
-- | ```
void :: forall f a. (Functor f) => f a -> f Unit
void = ($> unit)

foreign import mapArrayImpl :: forall a b. (a -> b) -> Array a -> Array b
