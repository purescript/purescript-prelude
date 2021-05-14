module Data.HeytingAlgebra.Laws where

import Prelude

import Data.HeytingAlgebra (ff, implies, tt)
import Laws.Algebraic as Laws

-- | `a || (b || c) = (a || b) || c`
associativeDisj :: forall a. Eq a => HeytingAlgebra a => a -> a -> a -> Boolean
associativeDisj = Laws.associative (||)

-- | `a && (b && c) = (a && b) && c`
associativeConj :: forall a. Eq a => HeytingAlgebra a => a -> a -> a -> Boolean
associativeConj = Laws.associative (&&)

-- | `a || b = b || a`
commutativeDisj :: forall a. Eq a => HeytingAlgebra a => a -> a -> Boolean
commutativeDisj = Laws.commutative (||)

-- | `a && b = b && a`
commutativeConj :: forall a. Eq a => HeytingAlgebra a => a -> a -> Boolean
commutativeConj = Laws.commutative (&&)

-- | `a || (a && b) = a && (a || b) = a`
absorption :: forall a. Eq a => HeytingAlgebra a => a -> a -> Boolean
absorption = Laws.absorption (||) (&&)

-- | `a || a = a`
idempotentDisj :: forall a. Eq a => HeytingAlgebra a => a -> a -> Boolean
idempotentDisj = Laws.idempotent (||)

-- | `a && a = a`
idempotentConj :: forall a. Eq a => HeytingAlgebra a => a -> a -> Boolean
idempotentConj = Laws.idempotent (&&)

-- | `a || ff = a`
identityDisj :: forall a. Eq a => HeytingAlgebra a => a -> Boolean
identityDisj = Laws.identity (||) ff

-- | `a && tt = a`
identityConj :: forall a. Eq a => HeytingAlgebra a => a -> Boolean
identityConj = Laws.identity (&&) tt

-- | ``a `implies` a = tt``
reflexiveImplies :: forall a. Eq a => HeytingAlgebra a => a -> Boolean
reflexiveImplies a = (a `implies` a) == tt

-- | ``(a && b) `implies` (b && c) = a `implies` c``
transitiveImplies :: forall a. Eq a => HeytingAlgebra a => a -> a -> a -> Boolean
transitiveImplies a b c = (a && b) `implies` (b && c) == a `implies` c

-- | ``(a `implies` b) || (b `implies` a)``
totalImplies :: forall a. HeytingAlgebra a => a -> a -> a
totalImplies a b = (a `implies` b) || (b `implies` a)

-- | ``a `implies` (b && c) = (a `implies` b) && (a `implies` c)``
leftDistributiveImplies :: forall a. Eq a => HeytingAlgebra a => a -> a -> a -> Boolean
leftDistributiveImplies = Laws.leftDistributive implies (&&)

-- | ``not a = a `implies` ff``
complemented :: forall a. Eq a => HeytingAlgebra a => a -> Boolean
complemented a = not a == (a `implies` ff)
