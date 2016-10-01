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
  , class GenericEq
  , genericEq'
  , genericEq
  , class GenericOrd
  , genericCompare'
  , genericCompare
  , class GenericSemigroup
  , genericAppend'
  , genericAppend
  , class GenericMonoid
  , genericMempty'
  , genericMempty
  ) where

import Prelude

import Data.Monoid (class Monoid, mempty)

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

class GenericEq a where
  genericEq' :: a -> a -> Boolean

instance genericEqNoConstructors :: GenericEq NoConstructors where
  genericEq' _ _ = true

instance genericEqNoArguments :: GenericEq NoArguments where
  genericEq' _ _ = true

instance genericEqSum :: (GenericEq a, GenericEq b) => GenericEq (Sum a b) where
  genericEq' (Inl a1) (Inl a2) = genericEq' a1 a2
  genericEq' (Inr b1) (Inr b2) = genericEq' b1 b2
  genericEq' _ _ = false

instance genericEqProduct :: (GenericEq a, GenericEq b) => GenericEq (Product a b) where
  genericEq' (Product a1 b1) (Product a2 b2) = genericEq' a1 a2 && genericEq' b1 b2

instance genericEqConstructor :: GenericEq a => GenericEq (Constructor name a) where
  genericEq' (Constructor a1) (Constructor a2) = genericEq' a1 a2

instance genericEqArgument :: Eq a => GenericEq (Argument a) where
  genericEq' (Argument a1) (Argument a2) = a1 == a2

instance genericEqRec :: GenericEq a => GenericEq (Rec a) where
  genericEq' (Rec a1) (Rec a2) = genericEq' a1 a2

instance genericEqField :: GenericEq a => GenericEq (Field name a) where
  genericEq' (Field a1) (Field a2) = genericEq' a1 a2

-- | A `Generic` implementation of the `eq` member from the `Eq` type class.
genericEq :: forall a rep. (Generic a rep, GenericEq rep) => a -> a -> Boolean
genericEq x y = genericEq' (from x) (from y)

class GenericOrd a where
  genericCompare' :: a -> a -> Ordering

instance genericOrdNoConstructors :: GenericOrd NoConstructors where
  genericCompare' _ _ = EQ

instance genericOrdNoArguments :: GenericOrd NoArguments where
  genericCompare' _ _ = EQ

instance genericOrdSum :: (GenericOrd a, GenericOrd b) => GenericOrd (Sum a b) where
  genericCompare' (Inl a1) (Inl a2) = genericCompare' a1 a2
  genericCompare' (Inr b1) (Inr b2) = genericCompare' b1 b2
  genericCompare' (Inl b1) (Inr b2) = LT
  genericCompare' (Inr b1) (Inl b2) = GT

instance genericOrdProduct :: (GenericOrd a, GenericOrd b) => GenericOrd (Product a b) where
  genericCompare' (Product a1 b1) (Product a2 b2) =
    case genericCompare' a1 a2 of
      EQ -> genericCompare' b1 b2
      other -> other

instance genericOrdConstructor :: GenericOrd a => GenericOrd (Constructor name a) where
  genericCompare' (Constructor a1) (Constructor a2) = genericCompare' a1 a2

instance genericOrdArgument :: Ord a => GenericOrd (Argument a) where
  genericCompare' (Argument a1) (Argument a2) = compare a1 a2

instance genericOrdRec :: GenericOrd a => GenericOrd (Rec a) where
  genericCompare' (Rec a1) (Rec a2) = genericCompare' a1 a2

instance genericOrdField :: GenericOrd a => GenericOrd (Field name a) where
  genericCompare' (Field a1) (Field a2) = genericCompare' a1 a2

-- | A `Generic` implementation of the `compare` member from the `Ord` type class.
genericCompare :: forall a rep. (Generic a rep, GenericOrd rep) => a -> a -> Ordering
genericCompare x y = genericCompare' (from x) (from y)

class GenericSemigroup a where
  genericAppend' :: a -> a -> a

instance genericSemigroupNoConstructors :: GenericSemigroup NoConstructors where
  genericAppend' a _ = a

instance genericSemigroupNoArguments :: GenericSemigroup NoArguments where
  genericAppend' a _ = a

instance genericSemigroupProduct :: (GenericSemigroup a, GenericSemigroup b) => GenericSemigroup (Product a b) where
  genericAppend' (Product a1 b1) (Product a2 b2) =
    Product (genericAppend' a1 a2) (genericAppend' b1 b2)

instance genericSemigroupConstructor :: GenericSemigroup a => GenericSemigroup (Constructor name a) where
  genericAppend' (Constructor a1) (Constructor a2) = Constructor (genericAppend' a1 a2)

instance genericSemigroupArgument :: Semigroup a => GenericSemigroup (Argument a) where
  genericAppend' (Argument a1) (Argument a2) = Argument (append a1 a2)

instance genericSemigroupRec :: GenericSemigroup a => GenericSemigroup (Rec a) where
  genericAppend' (Rec a1) (Rec a2) = Rec (genericAppend' a1 a2)

instance genericSemigroupField :: GenericSemigroup a => GenericSemigroup (Field name a) where
  genericAppend' (Field a1) (Field a2) = Field (genericAppend' a1 a2)

-- | A `Generic` implementation of the `append` member from the `Semigroup` type class.
genericAppend :: forall a rep. (Generic a rep, GenericSemigroup rep) => a -> a -> a
genericAppend x y = to (genericAppend' (from x) (from y))

class GenericMonoid a where
  genericMempty' :: a

instance genericMonoidNoArguments :: GenericMonoid NoArguments where
  genericMempty' = NoArguments

instance genericMonoidProduct :: (GenericMonoid a, GenericMonoid b) => GenericMonoid (Product a b) where
  genericMempty' = Product genericMempty' genericMempty'

instance genericMonoidConstructor :: GenericMonoid a => GenericMonoid (Constructor name a) where
  genericMempty' = Constructor genericMempty'

instance genericMonoidArgument :: Monoid a => GenericMonoid (Argument a) where
  genericMempty' = Argument mempty

instance genericMonoidRec :: GenericMonoid a => GenericMonoid (Rec a) where
  genericMempty' = Rec genericMempty'

instance genericMonoidField :: GenericMonoid a => GenericMonoid (Field name a) where
  genericMempty' = Field genericMempty'

-- | A `Generic` implementation of the `mempty` member from the `Monoid` type class.
genericMempty :: forall a rep. (Generic a rep, GenericMonoid rep) => a
genericMempty = to genericMempty'
