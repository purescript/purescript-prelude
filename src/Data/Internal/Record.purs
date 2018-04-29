module Data.Internal.Record where

-- | *Really* unsafely get a value from a record. You really shouldn't be using
-- | this function unless you know what you're doing.
foreign import unsafeGet :: forall a rs. String -> Record rs -> a

-- | *Really* unsafely insert a value into a record. Again, you really
-- | shouldn't use this function.
foreign import unsafeInsert
  :: forall a ra rb
   . String
  -> a
  -> Record ra
  -> Record rb
