module Data.Unit where

import Data.BooleanAlgebra (BooleanAlgebra)
import Data.BoundedOrd (Bounded, Eq, Ord, BoundedOrd)
import Data.Num (Semiring, ModuloSemiring, DivisionRing, Ring, Num)
import Data.Ordering (Ordering(EQ))
import Data.Semigroup (Semigroup)
import Data.Show (Show)

-- | The `Unit` type has a single inhabitant, called `unit`. It represents
-- | values with no computational content.
-- |
-- | `Unit` is often used, wrapped in a monadic type constructor, as the
-- | return type of a computation where only
-- | the _effects_ are important.
newtype Unit = Unit {}

-- | `unit` is the sole inhabitant of the `Unit` type.
unit :: Unit
unit = Unit {}

instance eqUnit :: Eq Unit where
  eq _ _ = true

instance ordUnit :: Ord Unit where
  compare _ _ = EQ

instance boundedUnit :: Bounded Unit where
  top = unit
  bottom = unit

instance boundedOrdUnit :: BoundedOrd Unit

instance semigroupUnit :: Semigroup Unit where
  append _ _ = unit

instance semiringUnit :: Semiring Unit where
  add _ _ = unit
  zero = unit
  mul _ _ = unit
  one = unit

instance moduloSemiringUnit :: ModuloSemiring Unit where
  div _ _ = unit
  mod _ _ = unit

instance divisionRingUnit :: DivisionRing Unit

instance ringUnit :: Ring Unit where
  sub _ _ = unit

instance numUnit :: Num Unit

instance booleanAlgebraUnit :: BooleanAlgebra Unit where
  conj _ _ = unit
  disj _ _ = unit
  not _ = unit

instance showUnit :: Show Unit where
  show _ = "unit"
