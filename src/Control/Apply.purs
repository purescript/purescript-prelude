module Control.Apply
  ( Apply
  , apply
  , (<*>)
  , (*>)
  , (<*)
  , liftA2
  , liftA3
  , liftA4
  , liftA5
  , module Data.Functor
  ) where

import Data.Category (id)
import Data.Function (const)
import Data.Functor

-- | The `Apply` class provides the `(<*>)` which is used to apply a function
-- | to an argument under a type constructor.
-- |
-- | `Apply` can be used to lift functions of two or more arguments to work on
-- | values wrapped with the type constructor `f`. It might also be understood
-- | in terms of the `lift2` function:
-- |
-- | ```purescript
-- | lift2 :: forall f a b c. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
-- | lift2 f a b = f <$> a <*> b
-- | ```
-- |
-- | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
-- | the function application operator `($)` to arguments wrapped with the
-- | type constructor `f`.
-- |
-- | Instances must satisfy the following law in addition to the `Functor`
-- | laws:
-- |
-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
-- |
-- | Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
class (Functor f) <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

instance applyFn :: Apply ((->) r) where
  apply f g x = f x (g x)

instance applyArray :: Apply Array where
  apply = applyArrayImpl

foreign import applyArrayImpl :: forall a b. Array (a -> b) -> Array a -> Array b

infixl 4 <*>
infixl 4 <*
infixl 4 *>

-- | `(<*>)` is an alias for `apply`.
(<*>) :: forall f a b. (Apply f) => f (a -> b) -> f a -> f b
(<*>) = apply

-- | Combine two effectful actions, keeping only the result of the first.
(<*) :: forall a b f. (Apply f) => f a -> f b -> f a
(<*) a b = const <$> a <*> b

-- | Combine two effectful actions, keeping only the result of the second.
(*>) :: forall a b f. (Apply f) => f a -> f b -> f b
(*>) a b = const id <$> a <*> b

-- | Lift a function of two arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
liftA2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Lift a function of three arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
liftA3 :: forall a b c d f. (Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

-- | Lift a function of four arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
liftA4 :: forall a b c d e f. (Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

-- | Lift a function of five arguments to a function which accepts and returns
-- | values wrapped with the type constructor `f`.
liftA5 :: forall a b c d e f g. (Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
