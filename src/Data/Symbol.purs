module Data.Symbol
  ( class IsSymbol
  , reflectSymbol
  , reifySymbol
  ) where

import Type.Proxy (Proxy(..))

-- | A class for known symbols
class IsSymbol :: Symbol -> Constraint
class IsSymbol @sym where
  reflectSymbol :: String

-- local definition for use in `reifySymbol`
foreign import unsafeCoerce :: forall a b. a -> b

reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => Proxy sym -> r) -> r
reifySymbol s f = coerce f { reflectSymbol: s }
  where
  coerce
    :: (forall sym1. IsSymbol sym1 => r)
    -> { reflectSymbol :: String }
    -> r
  coerce = unsafeCoerce
