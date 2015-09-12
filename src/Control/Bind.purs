module Control.Bind
  ( Bind
  , bind
  , (>>=)
  , (=<<)
  , (>=>)
  , (<=<)
  , join
  , ifM
  , module Control.Apply
  ) where

import Data.Category (id)
import Control.Apply

-- | The `Bind` type class extends the [`Apply`](#apply) type class with a
-- | "bind" operation `(>>=)` which composes computations in sequence, using
-- | the return value of one computation to determine the next computation.
-- |
-- | The `>>=` operator can also be expressed using `do` notation, as follows:
-- |
-- | ```purescript
-- | x >>= f = do y <- x
-- |              f y
-- | ```
-- |
-- | where the function argument of `f` is given the name `y`.
-- |
-- | Instances must satisfy the following law in addition to the `Apply`
-- | laws:
-- |
-- | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
-- |
-- | Associativity tells us that we can regroup operations which use `do`
-- | notation so that we can unambiguously write, for example:
-- |
-- | ```purescript
-- | do x <- m1
-- |    y <- m2 x
-- |    m3 x y
-- | ```
class (Apply m) <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

instance bindFn :: Bind ((->) r) where
  bind m f x = f (m x) x

instance bindArray :: Bind Array where
  bind = bindArrayImpl

foreign import bindArrayImpl :: forall a b. Array a -> (a -> Array b) -> Array b

infixl 1 >>=
infixr 1 =<<
infixr 1 >=>
infixr 1 <=<

-- | `(>>=)` is an alias for `bind`.
(>>=) :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
(>>=) = bind

-- | A version of `(>>=)` with its arguments flipped.
(=<<) :: forall a b m. (Bind m) => (a -> m b) -> m a -> m b
(=<<) f m = m >>= f

-- | Forwards Kleisli composition.
-- |
-- | For example:
-- |
-- | ```purescript
-- | import Data.Array (head, tail)
-- |
-- | third = tail >=> tail >=> head
-- | ```
(>=>) :: forall a b c m. (Bind m) => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a = f a >>= g

-- | Backwards Kleisli composition.
(<=<) :: forall a b c m. (Bind m) => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) f g a = f =<< g a

-- | Collapse two applications of a monadic type constructor into one.
join :: forall a m. (Bind m) => m (m a) -> m a
join = (>>= id)

-- | Execute a monadic action if a condition holds.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = ifM ((< 0.5) <$> random)
-- |          (trace "Heads")
-- |          (trace "Tails")
-- | ```
ifM :: forall a m. (Bind m) => m Boolean -> m a -> m a -> m a
ifM cond t f = cond >>= \cond' -> if cond' then t else f
