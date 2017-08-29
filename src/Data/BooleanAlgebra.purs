module Data.BooleanAlgebra
  ( class BooleanAlgebra
  , module Data.HeytingAlgebra
  , xor
  , (⊕)
  ) where

import Data.HeytingAlgebra (class HeytingAlgebra, ff, tt, implies, conj, disj, not, (&&), (||))
import Data.Unit (Unit)

-- | The `BooleanAlgebra` type class represents types that behave like boolean
-- | values.
-- |
-- | Instances should satisfy the following laws in addition to the
-- | `HeytingAlgebra` law:
-- |
-- | - Excluded middle:
-- |   - `a || not a = tt`
class HeytingAlgebra a <= BooleanAlgebra a

instance booleanAlgebraBoolean :: BooleanAlgebra Boolean
instance booleanAlgebraUnit :: BooleanAlgebra Unit
instance booleanAlgebraFn :: BooleanAlgebra b => BooleanAlgebra (a -> b)

xor ∷ ∀ a. HeytingAlgebra a ⇒ a → a → a
xor p q = (p `disj` q) `conj` (not (p `conj` q))

infixr 2 xor as ⊕
