module Data.Semiring.Laws where

import Prelude

import Laws.Algebraic as Laws

-- | `(a + b) + c = a + (b + c)`
associativeAdd :: forall a. Eq a => Semiring a => a -> a -> a -> Boolean
associativeAdd = Laws.associative (+)

-- | `zero + a = a + zero = a`
identityAdd :: forall a. Eq a => Semiring a => a -> Boolean
identityAdd = Laws.identity (+) zero

-- | `a + b = b + a`
commutativeAdd :: forall a. Eq a => Semiring a => a -> a -> Boolean
commutativeAdd = Laws.commutative (+)

-- | `(a * b) * c = a * (b * c)`
associativeMul :: forall a. Eq a => Semiring a => a -> a -> a -> Boolean
associativeMul = Laws.associative (*)

-- | `one * a = a * one = a`
identityMul :: forall a. Eq a => Semiring a => a -> Boolean
identityMul = Laws.identity (*) one

-- | `a * (b + c) = (a * b) + (a * c)`
leftDistributive :: forall t7. Eq t7 => Semiring t7 => t7 -> t7 -> t7 -> Boolean
leftDistributive = Laws.leftDistributive (*) (+)

-- | `(a + b) * c = (a * c) + (b * c)`
rightDistributive :: forall a. Eq a => Semiring a => a -> a -> a -> Boolean
rightDistributive = Laws.rightDistributive (*) (+)

-- | `zero * a = a * zero = zero`
annihilation :: forall a. Eq a => Semiring a => a -> Boolean
annihilation a = zero * a == zero && a * zero == zero
