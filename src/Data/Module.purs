module Data.Module where

import Data.Ring (zero, add, sub, mul)
import Data.CommutativeRing (class CommutativeRing)
import Data.Unit (Unit, unit)

-- | The `Module` class is for types that support addition as well as
-- | multiplication by a scalar on the left.
-- |
-- | If you want a vector space, simply specify an additional `Field s`
-- | constraint.
-- |
-- | Instances must satisfy the following laws in addition to the
-- | `CommutativeRing` laws:
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
class (CommutativeRing s) <= Module m s | m -> s where
  mzero :: m
  madd :: m -> m -> m
  msub :: m -> m -> m
  mmul :: s -> m -> m

infixl 6 madd as $+
infixl 6 msub as $-
infixl 7 mmul as $*

-- | `mmulFlipped` is `mmul` with the arguments flipped. For example:
-- |
-- | ```purescript
-- | twiceAsLong = (_ *$ 2.0)
-- | ```
mmulFlipped :: forall m s. Module m s => m -> s -> m
mmulFlipped a b = b $* a

infixl 7 mmulFlipped as *$

instance moduleUnit :: (CommutativeRing s) => Module Unit s where
  mzero = unit
  madd _ _ = unit
  msub _ _ = unit
  mmul _ _ = unit

instance moduleInt :: Module Int Int where
  mzero = zero
  madd = add
  msub = sub
  mmul = mul

instance moduleNumber :: Module Number Number where
  mzero = zero
  madd = add
  msub = sub
  mmul = mul
