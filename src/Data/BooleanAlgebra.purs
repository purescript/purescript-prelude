module Data.BooleanAlgebra
  ( class BooleanAlgebra, conj, disj, not
  , (&&), (||)
  ) where

import Data.Bounded (class Bounded)
import Data.Unit (Unit, unit)

-- | The `BooleanAlgebra` type class represents types that behave like boolean
-- | values.
-- |
-- | Instances should satisfy the following laws in addition to the `Bounded`
-- | laws:
-- |
-- | - Associativity:
-- |   - `a || (b || c) = (a || b) || c`
-- |   - `a && (b && c) = (a && b) && c`
-- | - Commutativity:
-- |   - `a || b = b || a`
-- |   - `a && b = b && a`
-- | - Distributivity:
-- |   - `a && (b || c) = (a && b) || (a && c)`
-- |   - `a || (b && c) = (a || b) && (a || c)`
-- | - Identity:
-- |   - `a || bottom = a`
-- |   - `a && top = a`
-- | - Idempotent:
-- |   - `a || a = a`
-- |   - `a && a = a`
-- | - Absorption:
-- |   - `a || (a && b) = a`
-- |   - `a && (a || b) = a`
-- | - Annhiliation:
-- |   - `a || top = top`
-- | - Complementation:
-- |   - `a && not a = bottom`
-- |   - `a || not a = top`
class Bounded a <= BooleanAlgebra a where
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

infixr 3 conj as &&
infixr 2 disj as ||

instance booleanAlgebraBoolean :: BooleanAlgebra Boolean where
  conj = boolConj
  disj = boolDisj
  not = boolNot

instance booleanAlgebraUnit :: BooleanAlgebra Unit where
  conj _ _ = unit
  disj _ _ = unit
  not _ = unit

instance booleanAlgebraFn :: BooleanAlgebra b => BooleanAlgebra (a -> b) where
  conj fx fy a = fx a `conj` fy a
  disj fx fy a = fx a `disj` fy a
  not fx a = not (fx a)

foreign import boolConj :: Boolean -> Boolean -> Boolean
foreign import boolDisj :: Boolean -> Boolean -> Boolean
foreign import boolNot :: Boolean -> Boolean
