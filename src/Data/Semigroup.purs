module Data.Semigroup
  ( Semigroup
  , append
  , (<>)
  ) where

-- | The `Semigroup` type class identifies an associative operation on a type.
-- |
-- | Instances are required to satisfy the following law:
-- |
-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
-- |
-- | One example of a `Semigroup` is `String`, with `(<>)` defined as string
-- | concatenation.
class Semigroup a where
  append :: a -> a -> a

infixr 5 <>

-- | `(<>)` is an alias for `append`.
(<>) :: forall s. (Semigroup s) => s -> s -> s
(<>) = append

instance semigroupFn :: (Semigroup t) => Semigroup (s -> t) where
  append f g x = f x `append` g x

instance semigroupArray :: Semigroup (Array a) where
  append = appendArray

instance semigroupString :: Semigroup String where
  append = appendString

foreign import appendArray :: forall a. Array a -> Array a -> Array a
foreign import appendString :: String -> String -> String
