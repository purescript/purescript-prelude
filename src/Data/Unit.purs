module Data.Unit where

import Data.Show (class Show)

-- | The `Unit` type has a single inhabitant, called `unit`. It represents
-- | values with no computational content.
-- |
-- | `Unit` is often used, wrapped in a monadic type constructor, as the
-- | return type of a computation where only the _effects_ are important.
-- |
-- | You can think of `Unit` as being analogous to the `void` keyword in
-- | other languages, such as C, Java, etc., which indicates that the
-- | function does not return any _meaningful_ data.
foreign import data Unit :: Type

-- | `unit` is the sole inhabitant of the `Unit` type.
-- |
-- | Use this to create a _value_ of the `Unit` type.
foreign import unit :: Unit

instance showUnit :: Show Unit where
  show _ = "unit"
