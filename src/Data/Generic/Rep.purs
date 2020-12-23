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
  ) where

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

-- | The `Generic` class asserts the existence of a type function from types
-- | to their representations using the type constructors defined in this module.
class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep
