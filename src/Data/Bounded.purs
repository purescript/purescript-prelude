module Data.Bounded
  ( Bounded
  , top
  , bottom
  ) where

-- | The `Bounded` type class represents types that are finite.
-- |
-- | Although there are no "internal" laws for `Bounded`, every value of `a`
-- | should be considered less than or equal to `top` by some means, and greater
-- | than or equal to `bottom`.
-- |
-- | The lack of explicit `Ord` constraint allows flexibility in the use of
-- | `Bounded` so it can apply to total and partially ordered sets, boolean
-- | algebras, etc.
class Bounded a where
  top :: a
  bottom :: a

instance boundedFn :: (Bounded b) => Bounded (a -> b) where
  top _ = top
  bottom _ = bottom

instance boundedBoolean :: Bounded Boolean where
  top = true
  bottom = false

-- | Integers fall within the Int32 range in the JavaScript backend, but may
-- | differ in other compliation targets.
instance boundedInt :: Bounded Int where
  top = topInt
  bottom = bottomInt

-- | Characters fall within the Unicode range.
instance boundedChar :: Bounded Char where
  top = topChar
  bottom = bottomChar

foreign import topInt :: Int
foreign import bottomInt :: Int
foreign import topChar :: Char
foreign import bottomChar :: Char
