module Data.BooleanAlgebra
  ( BooleanAlgebra
  , conj
  , disj
  , not
  , (&&)
  , (||)
  ) where

import Data.Bounded (Bounded)

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
class (Bounded a) <= BooleanAlgebra a where
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a

instance booleanAlgebraFn :: (BooleanAlgebra b) => BooleanAlgebra (a -> b) where
  conj f g x = f x `conj` g x
  disj f g x = f x `disj` g x
  not f x = not (f x)

instance booleanAlgebraBoolean :: BooleanAlgebra Boolean where
  conj = conjBooleanImpl
  disj = disjBooleanImpl
  not = notBooleanImpl

infixr 3 &&
infixr 2 ||

-- | `(&&)` is an alias for `conj`.
(&&) :: forall a. (BooleanAlgebra a) => a -> a -> a
(&&) = conj

-- | `(||)` is an alias for `disj`.
(||) :: forall a. (BooleanAlgebra a) => a -> a -> a
(||) = disj

foreign import conjBooleanImpl :: Boolean -> Boolean -> Boolean
foreign import disjBooleanImpl :: Boolean -> Boolean -> Boolean
foreign import notBooleanImpl :: Boolean -> Boolean
