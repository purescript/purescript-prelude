module Data.Debugged.Generic
  ( class GenericDebug
  , genericDebug'
  , genericDebug
  , class GenericDebugArgs
  , genericDebugArgs
  ) where

import Prelude

import Data.Debugged.Class (class Debug, debugged)
import Data.Debugged.Type as D
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

-- | A generic implementation of the `Debug` type class.
genericDebug :: forall a rep.
  Generic a rep =>
  GenericDebug rep =>
  a -> D.Repr
genericDebug = genericDebug' <<< from

class GenericDebug rep where
  genericDebug' :: rep -> D.Repr

instance genericDebugNoConstructors :: GenericDebug NoConstructors where
  genericDebug' x = genericDebug' x

instance genericDebugConstructor
  :: (GenericDebugArgs a, IsSymbol name) => GenericDebug (Constructor name a) where
  genericDebug' (Constructor a) =
    D.constructor
      (reflectSymbol (SProxy :: SProxy name))
      (genericDebugArgs a)

instance genericDebugSum :: (GenericDebug a, GenericDebug b) => GenericDebug (Sum a b) where
  genericDebug' (Inl a) = genericDebug' a
  genericDebug' (Inr b) = genericDebug' b

class GenericDebugArgs rep where
  genericDebugArgs :: rep -> Array D.Repr

instance genericDebugArgsArgument :: Debug a => GenericDebugArgs (Argument a) where
  genericDebugArgs (Argument a) = [debugged a]

instance genericDebugArgsProduct :: (GenericDebugArgs a, GenericDebugArgs b) => GenericDebugArgs (Product a b) where
  genericDebugArgs (Product a b) = genericDebugArgs a <> genericDebugArgs b

instance genericDebugArgsNoArguments :: GenericDebugArgs NoArguments where
  genericDebugArgs NoArguments = []
