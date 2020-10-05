-- | The `Proxy` type and values are for situations where type information is
-- | required for an input to determine the type of an output, but where it is
-- | not possible or convenient to provide a _value_ for the input.
-- |
-- | A hypothetical example: if you have a class that is used to handle the
-- | result of an AJAX request, you may want to use this information to set the
-- | expected content type of the request, so you might have a class something
-- | like this:
-- |
-- | ``` purescript
-- | class AjaxResponse a where
-- |   responseType :: a -> ResponseType
-- |   fromResponse :: Foreign -> a
-- | ```
-- |
-- | The problem here is `responseType` requires a value of type `a`, but we
-- | won't have a value of that type until the request has been completed. The
-- | solution is to use a `Proxy` type instead:
-- |
-- | ``` purescript
-- | class AjaxResponse a where
-- |   responseType :: Proxy a -> ResponseType
-- |   fromResponse :: Foreign -> a
-- | ```
-- |
-- | We can now call `responseType (Proxy :: Proxy SomeContentType)` to produce
-- | a `ResponseType` for `SomeContentType` without having to construct some
-- | empty version of `SomeContentType` first. In situations like this where
-- | the `Proxy` type can be statically determined, it is recommended to pull
-- | out the definition to the top level and make a declaration like:
-- |
-- | ``` purescript
-- | _SomeContentType :: Proxy SomeContentType
-- | _SomeContentType = Proxy
-- | ```
-- |
-- | That way the proxy value can be used as `responseType _SomeContentType`
-- | for improved readability. However, this is not always possible, sometimes
-- | the type required will be determined by a type variable. As PureScript has
-- | scoped type variables, we can do things like this:
-- |
-- | ``` purescript
-- | makeRequest :: URL -> ResponseType -> Aff _ Foreign
-- | makeRequest = ...
-- |
-- | fetchData :: forall a. (AjaxResponse a) => URL -> Aff _ a
-- | fetchData url = fromResponse <$> makeRequest url (responseType (Proxy :: Proxy a))
-- | ```
module Type.Proxy where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind, class Discard)
import Control.Monad (class Monad)

import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.Bounded (class Bounded)
import Data.CommutativeRing (class CommutativeRing)
import Data.Eq (class Eq)
import Data.Functor (class Functor)
import Data.HeytingAlgebra (class HeytingAlgebra)
import Data.Ord (class Ord)
import Data.Ring (class Ring)
import Data.Semiring (class Semiring)
import Data.Show (class Show, show)

-- | Proxy type for all `kind`s.
data Proxy :: forall k. k -> Type
data Proxy a = Proxy

derive instance eqProxy :: Eq (Proxy a)

derive instance functorProxy :: Functor Proxy

derive instance ordProxy :: Ord (Proxy a)

instance applicativeProxy :: Applicative Proxy where
  pure _ = Proxy

instance applyProxy :: Apply Proxy where
  apply _ _ = Proxy

instance bindProxy :: Bind Proxy where
  bind _ _ = Proxy

instance booleanAlgebraProxy :: BooleanAlgebra (Proxy a)

instance boundedProxy :: Bounded (Proxy a) where
  bottom = Proxy
  top = Proxy

instance commutativeRingProxy :: CommutativeRing (Proxy a)

instance discardProxy :: Discard (Proxy a) where
  discard = bind

instance heytingAlgebraProxy :: HeytingAlgebra (Proxy a) where
  conj _ _ = Proxy
  disj _ _ = Proxy
  implies _ _ = Proxy
  ff = Proxy
  not _ = Proxy
  tt = Proxy

instance monadProxy :: Monad Proxy

instance ringProxy :: Ring (Proxy a) where
  sub _ _ = Proxy

instance semigroupProxy :: Semigroup (Proxy a) where
  append _ _ = Proxy

instance semiringProxy :: Semiring (Proxy a) where
  add _ _ = Proxy
  mul _ _ = Proxy
  one = Proxy
  zero = Proxy

instance showProxy :: Show (Proxy a) where
  show _ = "Proxy"

-- | Value proxy for kind `Type -> Type` types.
-- | **Deprecated as of v0.14.0 PureScript release**: use `Proxy` instead.
data Proxy2 :: (Type -> Type) -> Type
data Proxy2 f = Proxy2

derive instance eqProxy2 :: Eq (Proxy2 a)

derive instance ordProxy2 :: Ord (Proxy2 a)

instance booleanAlgebraProxy2 :: BooleanAlgebra (Proxy2 a)

instance boundedProxy2 :: Bounded (Proxy2 a) where
  bottom = Proxy2
  top = Proxy2

instance commutativeRingProxy2 :: CommutativeRing (Proxy2 a)

instance discardProxy2 :: Discard (Proxy2 a) where
  discard = bind

instance heytingAlgebraProxy2 :: HeytingAlgebra (Proxy2 a) where
  conj _ _ = Proxy2
  disj _ _ = Proxy2
  implies _ _ = Proxy2
  ff = Proxy2
  not _ = Proxy2
  tt = Proxy2

instance ringProxy2 :: Ring (Proxy2 a) where
  sub _ _ = Proxy2

instance semigroupProxy2 :: Semigroup (Proxy2 a) where
  append _ _ = Proxy2

instance semiringProxy2 :: Semiring (Proxy2 a) where
  add _ _ = Proxy2
  mul _ _ = Proxy2
  one = Proxy2
  zero = Proxy2

instance showProxy2 :: Show (Proxy2 a) where
  show _ = "Proxy2"

-- | Value proxy for kind `Type -> Type -> Type` types.
-- | **Deprecated as of v0.14.0 PureScript release**: use `Proxy` instead.
data Proxy3 (a :: Type -> Type -> Type) = Proxy3

derive instance eqProxy3 :: Eq (Proxy3 a)

derive instance ordProxy3 :: Ord (Proxy3 a)

instance booleanAlgebraProxy3 :: BooleanAlgebra (Proxy3 a)

instance boundedProxy3 :: Bounded (Proxy3 a) where
  bottom = Proxy3
  top = Proxy3

instance commutativeRingProxy3 :: CommutativeRing (Proxy3 a)

instance discardProxy3 :: Discard (Proxy3 a) where
  discard = bind

instance heytingAlgebraProxy3 :: HeytingAlgebra (Proxy3 a) where
  conj _ _ = Proxy3
  disj _ _ = Proxy3
  implies _ _ = Proxy3
  ff = Proxy3
  not _ = Proxy3
  tt = Proxy3

instance ringProxy3 :: Ring (Proxy3 a) where
  sub _ _ = Proxy3

instance semigroupProxy3 :: Semigroup (Proxy3 a) where
  append _ _ = Proxy3

instance semiringProxy3 :: Semiring (Proxy3 a) where
  add _ _ = Proxy3
  mul _ _ = Proxy3
  one = Proxy3
  zero = Proxy3

instance showProxy3 :: Show (Proxy3 a) where
  show _ = "Proxy3"
