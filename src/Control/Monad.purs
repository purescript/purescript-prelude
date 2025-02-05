module Control.Monad
  ( class Monad
  , liftM1
  , whenM
  , whenM'
  , unlessM
  , unlessM'
  , ap
  , module Data.Functor
  , module Control.Apply
  , module Control.Applicative
  , module Control.Bind
  ) where

import Control.Applicative (class Applicative, liftA1, pure, unless, when)
import Control.Apply (class Apply, apply, (*>), (<*), (<*>))
import Control.Bind (class Bind, bind, join, (<=<), (=<<), (>=>), (>>=), ifM, ifM')
import Control.Category ((>>>))

import Data.HeytingAlgebra (not)
import Data.Function (($))
import Data.Functor (class Functor, map, void, ($>), (<#>), (<$), (<$>))
import Data.Unit (Unit, unit)
import Type.Proxy (Proxy)

-- | The `Monad` type class combines the operations of the `Bind` and
-- | `Applicative` type classes. Therefore, `Monad` instances represent type
-- | constructors which support sequential composition, and also lifting of
-- | functions of arbitrary arity.
-- |
-- | Instances must satisfy the following laws in addition to the
-- | `Applicative` and `Bind` laws:
-- |
-- | - Left Identity: `pure x >>= f = f x`
-- | - Right Identity: `x >>= pure = x`
class (Applicative m, Bind m) <= Monad m

instance monadFn :: Monad ((->) r)

instance monadArray :: Monad Array

instance monadProxy :: Monad Proxy

-- | `liftM1` provides a default implementation of `(<$>)` for any
-- | [`Monad`](#monad), without using `(<$>)` as provided by the
-- | [`Functor`](#functor)-[`Monad`](#monad) superclass relationship.
-- |
-- | `liftM1` can therefore be used to write [`Functor`](#functor) instances
-- | as follows:
-- |
-- | ```purescript
-- | instance functorF :: Functor F where
-- |   map = liftM1
-- | ```
liftM1 :: forall m a b. Monad m => (a -> b) -> m a -> m b
liftM1 f a = do
  a' <- a
  pure (f a')

-- | Perform a monadic action when a condition is true, where the conditional
-- | value is also in a monadic context.
whenM :: forall m. Monad m => m Boolean -> m Unit -> m Unit
whenM mb m = ifM mb m $ pure unit

-- | Perform a monadic action lazily when a condition is true, where the conditional
-- | value is also in a monadic context.
whenM' :: forall m a. Monad m => (a -> m Boolean) -> (a -> m Unit) -> a -> m Unit
whenM' mb m = ifM' mb m \_ -> pure unit

-- | Perform a monadic action unless a condition is true, where the conditional
-- | value is also in a monadic context.
unlessM :: forall m. Monad m => m Boolean -> m Unit -> m Unit
unlessM mb = whenM $ not <$> mb

-- | Perform a monadic action lazily unless a condition is true, where the conditional
-- | value is also in a monadic context.
unlessM' :: forall m a. Monad m => (a -> m Boolean) -> (a -> m Unit) -> a -> m Unit
unlessM' mb = whenM' \x -> mb x >>= not >>> pure

-- | `ap` provides a default implementation of `(<*>)` for any `Monad`, without
-- | using `(<*>)` as provided by the `Apply`-`Monad` superclass relationship.
-- |
-- | `ap` can therefore be used to write `Apply` instances as follows:
-- |
-- | ```purescript
-- | instance applyF :: Apply F where
-- |   apply = ap
-- | ```
-- Note: Only a `Bind` constraint is needed, but this can
-- produce loops when used with other default implementations
-- (i.e. `liftA1`).
-- See https://github.com/purescript/purescript-prelude/issues/232
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  pure (f' a')
