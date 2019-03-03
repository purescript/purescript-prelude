module Data.EuclideanRing.Laws where

import Prelude

-- | `one /= zero`
integralDomainIdentity :: forall proxy a. Eq a => EuclideanRing a => proxy a -> Boolean
integralDomainIdentity _ = one /= (zero :: a)

-- | if `a` and `b` are both nonzero then so is `a * b`
integralDomainDivision :: forall a. Eq a => EuclideanRing a => a -> a -> Boolean
integralDomainDivision a b
  | a /= zero && b /= zero = a * b /= zero
  | otherwise = true

-- | For all nonzero `a`, `degree a >= zero`
nonNegativeDegree :: forall a. Eq a => EuclideanRing a => a -> Boolean
nonNegativeDegree a
  | a /= zero = degree a >= zero
  | otherwise = true

-- | For all `a` and `b`, where `b` is nonzero, let `q = a / b` and
-- | ``r = a `mod` b``; then `a = q*b + r`, and also either `r = zero` or
-- | `degree r < degree b`.
divisionWithRem :: forall a. Eq a => EuclideanRing a => a -> a -> Boolean
divisionWithRem a b
  | b /= zero =
      let
        q = a / b
        r = a `mod` b
      in
        a == b * q + r && (r == zero || degree r < degree b)
  | otherwise =
      true

-- | For all nonzero `a` and `b`, `degree a <= degree (a * b)`
submultiplicativeDegree :: forall a. Eq a => EuclideanRing a => a -> a -> Boolean
submultiplicativeDegree a b
  | a /= zero && b /= zero = degree a < degree (a * b)
  | otherwise = true
