module Data.Symbol
  ( class IsSymbol
  , reflectSymbol
  , reifySymbol
  ) where

import Type.Proxy (Proxy(..))

-- | A class for known symbols
class IsSymbol (sym :: Symbol) where
  -- Note: Before v0.14.0, we did not have polykinds. Thus, we needed
  -- kind-specific proxy types to pass a Symbol around.
  -- Once v0.14.0 was released, we could use the kind-generic `Proxy` type
  -- instead. However, to reduce code breakage, we're using
  -- `forall proxy. proxy sym` here so that `SProxy` code will still compile.
  -- When PureScript makes a new breaking release after the v0.14.0 release,
  -- this type signature will be updated to `Proxy sym -> String`.
  reflectSymbol :: Proxy sym -> String

-- local definition for use in `reifySymbol`
foreign import unsafeCoerce :: forall a b. a -> b

reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => Proxy sym -> r) -> r
reifySymbol s f = coerce f { reflectSymbol: \_ -> s } Proxy where
  coerce
    :: (forall sym1. IsSymbol sym1              => Proxy sym1 -> r)
    -> { reflectSymbol :: Proxy "" -> String } -> Proxy ""   -> r
  coerce = unsafeCoerce
