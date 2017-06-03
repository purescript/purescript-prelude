module Data.Generic.Rep
  ( class Generic
  , to
  , from
  , NoConstructors
  , NoArguments(..)
  , Sum(..)
  , Product(..)
  , Constructor(..)
  , Argument(..)
  , Rec(..)
  , Field(..)
  ) where

import Data.Maybe (Maybe(..))

-- | A representation for types with no constructors.
data NoConstructors

-- | A representation for constructors with no arguments.
data NoArguments = NoArguments

-- | A representation for types with multiple constructors.
data Sum a b = Inl a | Inr b

-- | A representation for constructors with multiple fields.
data Product a b = Product a b

-- | A representation for constructors which includes the data constructor name
-- | as a type-level string.
newtype Constructor (name :: Symbol) a = Constructor a

-- | A representation for an argument in a data constructor.
newtype Argument a = Argument a

-- | A representation for records.
newtype Rec fields = Rec fields

-- | A representation for a record field which includes the field name
-- | as a type-level string.
newtype Field (field :: Symbol) a = Field a

-- | The `Generic` class asserts the existence of a type function from types
-- | to their representations using the type constructors defined in this module.
class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep

instance genericMaybe
  :: Generic (Maybe a) (Sum (Constructor "Nothing" NoArguments)
                            (Constructor "Just" (Argument a))) where
  to (Inl _) = Nothing
  to (Inr (Constructor (Argument a))) = Just a

  from Nothing = Inl (Constructor NoArguments)
  from (Just a) = Inr (Constructor (Argument a))

