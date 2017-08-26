module Data.Module where

import Data.Ring (class Ring, zero, add, sub, mul)
import Data.Unit (Unit, unit)

-- | The `LeftModule` class is for types that support addition as well as
-- | multiplication by a scalar on the left.
-- |
-- | If you want a vector space, simply specify an additional `Field s`
-- | constraint.
-- |
-- | Instances must satisfy the following laws in addition to the `Ring` laws:
-- |
-- | - Commutative monoid under addition:
-- |   - Associativity: `(a $+ b) $+ c = a $+ (b $+ c)`
-- |   - Identity: `mlzero $+ a = a $+ mlzero = a`
-- |   - Commutative: `a $+ b = b $+ a`
-- | - Multiplicative identity: `one $* a = a`
-- | - Multiplication distributes over addition:
-- |   - Left distributivity: `a $* (b $+ c) = (a $* b) $+ (a $* c)`
-- |   - Right distributivity: `(a + b) $* c = (a $* c) $+ (b $* c)`
-- | - Annihilation: `zero $* a = mlzero`
-- | - Compatibility: `(a * b) $* c = a $* (b $* c)`
class (Ring s) <= LeftModule m s | m -> s where
  mlzero :: m
  mladd :: m -> m -> m
  mlsub :: m -> m -> m
  mlmul :: s -> m -> m

infixl 6 mladd as $+
infixl 6 mlsub as $-
infixl 7 mlmul as $*

-- | The `RightModule` class is for types that support addition as well as
-- | multiplication by a scalar on the right.
-- |
-- | If you want a vector space, simply specify an additional `Field s`
-- | constraint.
-- |
-- | Instances must satisfy the following laws in addition to the `Ring` laws:
-- |
-- | - Commutative monoid under addition:
-- |   - Associativity: `(a +$ b) +$ c = a +$ (b +$ c)`
-- |   - Identity: `mlzero +$ a = a +$ mlzero = a`
-- |   - Commutative: `a +$ b = b +$ a`
-- | - Multiplicative identity: `a *$ one = a`
-- | - Multiplication distributes over addition:
-- |   - Left distributivity: `a *$ (a + b) = (a *$ c) +$ (b *$ c)`
-- |   - Right distributivity: `(a +$ b) *$ c = (a *$ c) +$ (b *$ c)`
-- | - Annihilation: `zero *$ a = mlzero`
-- | - Compatibility: `a *$ (b * c) = (a *$ b) *$ c`
class (Ring s) <= RightModule m s | m -> s where
  mrzero :: m
  mradd :: m -> m -> m
  mrsub :: m -> m -> m
  mrmul :: m -> s -> m

infixl 6 mradd as +$
infixl 6 mrsub as -$
infixl 7 mrmul as *$

instance leftModuleUnit :: (Ring s) => LeftModule Unit s where
  mlzero = unit
  mladd _ _ = unit
  mlsub _ _ = unit
  mlmul _ _ = unit

instance rightModuleUnit :: (Ring s) => RightModule Unit s where
  mrzero = unit
  mradd _ _ = unit
  mrsub _ _ = unit
  mrmul _ _ = unit

instance leftModuleInt :: LeftModule Int Int where
  mlzero = zero
  mladd = add
  mlsub = sub
  mlmul = mul

instance rightModuleInt :: RightModule Int Int where
  mrzero = zero
  mradd = add
  mrsub = sub
  mrmul = mul

instance leftModuleNumber :: LeftModule Number Number where
  mlzero = zero
  mladd = add
  mlsub = sub
  mlmul = mul

instance rightModuleNumber :: RightModule Number Number where
  mrzero = zero
  mradd = add
  mrsub = sub
  mrmul = mul
