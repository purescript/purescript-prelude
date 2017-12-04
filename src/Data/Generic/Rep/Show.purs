module Data.Generic.Rep.Show
  ( class GenericShow
  , genericShow'
  , genericShow
  , class GenericShowArgs
  , genericShowArgs
  , class GenericShowFields
  , genericShowFields
  ) where

import Prelude (class Show, show, (<>))
import Data.Foldable (intercalate)
import Data.Generic.Rep
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

class GenericShow a where
  genericShow' :: a -> String

class GenericShowArgs a where
  genericShowArgs :: a -> Array String

class GenericShowFields a where
  genericShowFields :: a -> Array String

instance genericShowNoConstructors :: GenericShow NoConstructors where
  genericShow' a = genericShow' a

instance genericShowArgsNoArguments :: GenericShowArgs NoArguments where
  genericShowArgs _ = []

instance genericShowSum :: (GenericShow a, GenericShow b) => GenericShow (Sum a b) where
  genericShow' (Inl a) = genericShow' a
  genericShow' (Inr b) = genericShow' b

instance genericShowArgsProduct
    :: (GenericShowArgs a, GenericShowArgs b)
    => GenericShowArgs (Product a b) where
  genericShowArgs (Product a b) = genericShowArgs a <> genericShowArgs b

instance genericShowFieldsProduct
    :: (GenericShowFields a, GenericShowFields b)
    => GenericShowFields (Product a b) where
  genericShowFields (Product a b) = genericShowFields a <> genericShowFields b

instance genericShowConstructor
  :: (GenericShowArgs a, IsSymbol name)
  => GenericShow (Constructor name a) where
  genericShow' (Constructor a) =
      case genericShowArgs a of
        [] -> ctor
        args -> "(" <> intercalate " " ([ctor] <> args) <> ")"
    where
      ctor :: String
      ctor = reflectSymbol (SProxy :: SProxy name)

instance genericShowArgsArgument :: Show a => GenericShowArgs (Argument a) where
  genericShowArgs (Argument a) = [show a]

instance genericShowArgsRec :: GenericShowFields a => GenericShowArgs (Rec a) where
  genericShowArgs (Rec a) = ["{ " <> intercalate ", " (genericShowFields a) <> " }"]

instance genericShowFieldsField
    :: (Show a, IsSymbol name)
    => GenericShowFields (Field name a) where
  genericShowFields (Field a) =
    [reflectSymbol (SProxy :: SProxy name) <> ": " <> show a]

instance genericShowFieldsNoArguments :: GenericShowFields NoArguments where
  genericShowFields _ = []

-- | A `Generic` implementation of the `show` member from the `Show` type class.
genericShow :: forall a rep. Generic a rep => GenericShow rep => a -> String
genericShow x = genericShow' (from x)
