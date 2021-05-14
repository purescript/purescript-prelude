module Data.VectorSpace where

import Data.EuclideanRing (div)
import Data.Field (class Field)
import Data.Module (class Module)
import Data.Unit (Unit, unit)

-- | The `VectorSpace` class is for modules that support division by a scalar.
-- |
-- | Instances must satisfy the following laws in addition to the `Field` and
-- | `Module` laws:
-- |
-- | - Multiplicative inverse: `a /$ (b / c) = a *$ (c / b)`
class (Field s, Module m s) <= VectorSpace m s | m -> s where
  mdiv :: m -> s -> m

infixl 7 mdiv as /$

instance vectorSpaceUnit :: (Field s) => VectorSpace Unit s where
  mdiv _ _ = unit

instance vectorSpaceNumber :: VectorSpace Number Number where
  mdiv = div
