module Data.HeytingAlgebra.Laws where

import Data.Eq (class Eq, (==))
import Data.HeytingAlgebra (class HeytingAlgebra, ff, implies, not, tt, (&&), (||))

-- | Checks associativity of the `disj` function for a given triple of values:
-- |
-- |     a || (b || c) = (a || b) || c
associativityDisj :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> a -> Boolean
associativityDisj a b c =
  (a || (b || c)) == ((a || b) || c)

-- | Checks associativity of the `conj` function for a given triple of values:
-- |
-- |     a && (b && c) = (a && b) && c
associativityConj :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> a -> Boolean
associativityConj a b c =
  (a && (b && c)) == ((a && b) && c)

-- | Checks commutativity of the `disj` function for a given pair of values.
-- |
-- |     a || b = b || a
commutativityDisj :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> Boolean
commutativityDisj a b =
  (a || b) == (b || a)

-- | Checks commutativity of the `conj` function for a given pair of values:
-- |
-- |     a && b = b && a
commutativityConj :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> Boolean
commutativityConj a b =
  (a && b) == (b && a)

-- | Checks that `disj` absorbs `conj` for a given pair of values:
-- |
-- |     a || (a && b) = a 
absorptionDisj :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> Boolean
absorptionDisj a b =
  (a || (a && b)) == a

-- | Checks that `conj` absorbs `disj` for a given pair of values:
-- |
-- |     a && (a || b) = a 
absorptionConj :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> Boolean
absorptionConj a b =
  (a && (a || b)) == a

-- | Checks that `disj` is idempotent for a given value:
-- |
-- |     a || a = a
idempotentDisj :: forall a. (Eq a, HeytingAlgebra a) => a -> Boolean
idempotentDisj a =
  (a || a) == a

-- | Checks that `conj` is idempotent for a given value:
-- |
-- |     a && a = a
idempotentConj :: forall a. (Eq a, HeytingAlgebra a) => a -> Boolean
idempotentConj a =
  (a && a) == a

-- | Checks that `ff` is the identity of `disj` for a given value:
-- |
-- |     a || ff = a
identityDisj :: forall a. (Eq a, HeytingAlgebra a) => a -> Boolean
identityDisj a =
  (a || ff) == a

-- | Checks that `tt` is the identity of `conj` for a given value:
-- |
-- |     a && tt = a
identityConj :: forall a. (Eq a, HeytingAlgebra a) => a -> Boolean
identityConj a =
  (a && tt) == a

-- | Checks the first implication law for a given value:
-- |
-- |     a `implies` a = tt
implication1 :: forall a. (Eq a, HeytingAlgebra a) => a -> Boolean
implication1 a =
  a `implies` a == tt

-- | Checks the second implication law for a given value:
-- |
-- |     a && (a `implies` b) = a && b
implication2 :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> Boolean
implication2 a b =
  (a && (a `implies` b)) == (a && b)

-- | Checks the third implication law for a given value:
-- |
-- |     b && (a `implies` b) = b
implication3 :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> Boolean
implication3 a b =
  (b && (a `implies` b)) == b

-- | Checks the fourth implication law for a given value:
-- |
-- |     a `implies` (b && c) = (a `implies` b) && (a `implies` c)
implication4 :: forall a. (Eq a, HeytingAlgebra a) => a -> a -> a -> Boolean
implication4 a b c =
  (a `implies` (b && c)) == ((a `implies` b) && (a `implies` c))

-- | Checks the complemented law for a given value:
-- |
-- |     not a = a `implies` ff
complemented :: forall a. (Eq a, HeytingAlgebra a) => a -> Boolean
complemented a =
  (not a) == (a `implies` ff)
