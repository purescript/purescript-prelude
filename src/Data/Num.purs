module Data.Num
  ( Num
  , module Data.DivisionRing
  ) where

import Data.DivisionRing

-- | The `Num` class is for types that are commutative fields.
-- |
-- | Instances must satisfy the following law in addition to the
-- | `DivisionRing` laws:
-- |
-- | - Commutative multiplication: `a * b = b * a`
class (DivisionRing a) <= Num a

instance numFn :: (Num b) => Num (a -> b)
instance numNumber :: Num Number
