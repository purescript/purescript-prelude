module Control.Monad
  ( Monad
  , liftM
  , ap
  , module Control.Applicative
  , module Control.Bind
  ) where

import Control.Applicative
import Control.Bind

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

-- | `liftM` provides a default implementation of `(<$>)` for any
-- | [`Monad`](#monad), without using `(<$>)` as provided by the
-- | [`Functor`](#functor)-[`Monad`](#monad) superclass relationship.
-- |
-- | `liftM` can therefore be used to write [`Functor`](#functor) instances
-- | as follows:
-- |
-- | ```purescript
-- | instance functorF :: Functor F where
-- |   map = liftM
-- | ```
liftM :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
liftM f a = do
  a' <- a
  pure (f a')

-- | `ap` provides a default implementation of `(<*>)` for any
-- | [`Monad`](#monad), without using `(<*>)` as provided by the
-- | [`Apply`](#apply)-[`Monad`](#monad) superclass relationship.
-- |
-- | `ap` can therefore be used to write [`Apply`](#apply) instances as
-- | follows:
-- |
-- | ```purescript
-- | instance applyF :: Apply F where
-- |   apply = ap
-- | ```
ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  pure (f' a')
