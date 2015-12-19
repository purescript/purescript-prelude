module Prelude.Internal where

-- | Applies a function to its argument.
-- |
-- | ```purescript
-- | length $ groupBy productCategory $ filter isInStock $ products
-- | ```
-- |
-- | is equivalent to:
-- |
-- | ```purescript
-- | length (groupBy productCategory (filter isInStock products))
-- | ```
-- |
-- | `($)` is different from `(#)` because it is right-infix instead of
-- | left: `a $ b $ c $ d x = a $ (b $ (c $ (d $ x))) = a (b (c (d x)))`
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

-- | Applies an argument to a function.
-- |
-- | ```purescript
-- | products # filter isInStock # groupBy productCategory # length
-- | ```
-- |
-- | is equivalent to:
-- |
-- | ```purescript
-- | length (groupBy productCategory (filter isInStock products))
-- | ```
-- |
-- | `(#)` is different from `($)` because it is left-infix instead of
-- | right: `x # a # b # c # d = (((x # a) # b) # c) # d = d (c (b (a x)))`
applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped x f = f x

infixl 1 applyFlipped as #
