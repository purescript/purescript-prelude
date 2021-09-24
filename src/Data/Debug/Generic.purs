-- | This module provides a mechanism for deriving `Debug` instances for types
-- | which export their constructors. To use it, simply derive a `Generic`
-- | instance and then use `genericDebug` in the `Debug` instance. For example:
-- |
-- | ```
-- | data MyType = [...]
-- | derive instance genericMyType :: Generic MyType _
-- | instance debugMyType :: Debug MyType where
-- |   debug genericDebug
-- | ```
-- |
-- | Note that the resulting `Debug` instance will expose details of the
-- | constructors. Therefore, this mechanism is not suitable for types which
-- | hide their constructors.
module Data.Debug.Generic
  ( genericDebug
  , class GenericDebug
  , genericDebug'
  , class GenericDebugArgs
  , genericDebugArgs
  ) where

import Control.Semigroupoid ((<<<))
import Data.Semigroup ((<>))
import Data.Debug.Class (class Debug, debug)
import Data.Debug.Type as D
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

-- | A generic implementation of the `Debug` type class.
genericDebug :: forall a rep.
  Generic a rep =>
  GenericDebug rep =>
  a -> D.Repr
genericDebug = genericDebug' <<< from

-- | This class is part of the machinery for deriving `Debug` instances; it is
-- | not intended to be used directly.
class GenericDebug rep where
  genericDebug' :: rep -> D.Repr

instance genericDebugNoConstructors :: GenericDebug NoConstructors where
  genericDebug' x = genericDebug' x

instance genericDebugConstructor
  :: (GenericDebugArgs a, IsSymbol name) => GenericDebug (Constructor name a) where
  genericDebug' (Constructor a) =
    D.constructor
      (reflectSymbol (Proxy :: Proxy name))
      (genericDebugArgs a)

instance genericDebugSum :: (GenericDebug a, GenericDebug b) => GenericDebug (Sum a b) where
  genericDebug' (Inl a) = genericDebug' a
  genericDebug' (Inr b) = genericDebug' b

-- | This class is part of the machinery for deriving `Debug` instances; it is
-- | not intended to be used directly.
class GenericDebugArgs rep where
  genericDebugArgs :: rep -> Array D.Repr

instance genericDebugArgsArgument :: Debug a => GenericDebugArgs (Argument a) where
  genericDebugArgs (Argument a) = [debug a]

instance genericDebugArgsProduct :: (GenericDebugArgs a, GenericDebugArgs b) => GenericDebugArgs (Product a b) where
  genericDebugArgs (Product a b) = genericDebugArgs a <> genericDebugArgs b

instance genericDebugArgsNoArguments :: GenericDebugArgs NoArguments where
  genericDebugArgs NoArguments = []
