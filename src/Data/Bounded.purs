module Data.Bounded (class Bounded, bottom, top) where

import Data.Unit (Unit, unit)

-- | The `Bounded` type class represents types that have an upper and lower
-- | boundary.
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

instance boundedBoolean :: Bounded Boolean where
  top = true
  bottom = false

instance boundedInt :: Bounded Int where
  top = topInt
  bottom = bottomInt

foreign import topInt :: Int
foreign import bottomInt :: Int

-- | Characters fall within the Unicode range.
instance boundedChar :: Bounded Char where
  top = topChar
  bottom = bottomChar

foreign import topChar :: Char
foreign import bottomChar :: Char

instance boundedUnit :: Bounded Unit where
  top = unit
  bottom = unit

instance boundedFn :: Bounded b => Bounded (a -> b) where
  top _ = top
  bottom _ = bottom
