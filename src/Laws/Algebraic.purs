module Laws.Algebraic where

import Prelude

-- | `id • a = a`
leftIdentity :: forall a id. Eq a => (id -> a -> a) -> id -> a -> Boolean
leftIdentity op id x = (id `op` x) == x

-- | `a • id = a`
rightIdentity :: forall a id. Eq a => (a -> id -> a) -> id -> a -> Boolean
rightIdentity op id x = (x `op` id) == x

-- | `id • a = a • id = a`
identity :: forall a. Eq a => (a -> a -> a) -> a -> a -> Boolean
identity op id x = leftIdentity op id x && rightIdentity op id x

-- | `a • (b • c) = (a • b) • c`
associative :: forall a. Eq a => (a -> a -> a) -> a -> a -> a -> Boolean
associative op a b c = (a `op` (b `op` c)) == ((a `op` b) `op` c)

-- | `a • b = b • a`
commutative :: forall a. Eq a => (a -> a -> a) -> a -> a -> Boolean
commutative op a b = (a `op` b) == (b `op` a)

-- | `a • (b ◆ c) = (a • b) ◆ (a • c)`
leftDistributive :: forall a. Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Boolean
leftDistributive op1 op2 a b c = a `op1` (b `op2` c) == (a `op1` b) `op2` (a `op1` c)

-- | `(a ◆ b) • c = (a • c) ◆ (b • c)`
rightDistributive :: forall a. Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Boolean
rightDistributive op1 op2 a b c = (a `op2` b) `op1` c == (a `op1` c) `op2` (b `op1` c)

-- | `a • a = a`
idempotent :: forall a. Eq a => (a -> a -> a) -> a -> a -> Boolean
idempotent op a b = a `op` a == a

-- | `a • (a ◆ b) = a ◆ (a • b) = a`
absorption :: forall a. Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> Boolean
absorption op1 op2 a b = (a `op1` (a `op2` b)) == a && (a `op2` (a `op1` b)) == a
