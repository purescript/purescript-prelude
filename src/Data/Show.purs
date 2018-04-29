module Data.Show (class Show, show) where

import Data.Internal.Record (unsafeGet)
import Data.RowList (RLProxy(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.RowList as RL

-- | The `Show` type class represents those types which can be converted into
-- | a human-readable `String` representation.
-- |
-- | While not required, it is recommended that for any expression `x`, the
-- | string `show x` be executable PureScript code which evaluates to the same
-- | value as the expression `x`.
class Show a where
  show :: a -> String

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

instance showInt :: Show Int where
  show = showIntImpl

instance showNumber :: Show Number where
  show = showNumberImpl

instance showChar :: Show Char where
  show = showCharImpl

instance showString :: Show String where
  show = showStringImpl

instance showArray :: Show a => Show (Array a) where
  show = showArrayImpl show

class ShowRecordFields
    (rowlist :: RL.RowList)
    (row :: # Type)
    focus
    | rowlist -> focus where
  showRecordFieldsImpl :: (RLProxy rowlist) -> Record row -> Array String

instance showRecordFieldsNil
    :: ShowRecordFields RL.Nil row focus where
  showRecordFieldsImpl _ _ = []

instance showRecordFieldsCons
    :: ( IsSymbol key
       , ShowRecordFields rowlistTail row subfocus
       , Show focus
       )
    => ShowRecordFields (RL.Cons key focus rowlistTail) row focus where
  showRecordFieldsImpl _ record = cons
    (join ": " [ key, show (unsafeGet' key record) ])
    (showRecordFieldsImpl (RLProxy :: RLProxy rowlistTail) record)
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus

instance showRecord
    :: ( RL.RowToList rs ls
       , ShowRecordFields ls rs focus
       )
    => Show (Record rs) where
  show record = case showRecordFieldsImpl (RLProxy :: RLProxy ls) record of
    []     -> "{}"
    fields -> join " " [ "{", join ", " fields, "}" ]

foreign import showIntImpl :: Int -> String
foreign import showNumberImpl :: Number -> String
foreign import showCharImpl :: Char -> String
foreign import showStringImpl :: String -> String
foreign import showArrayImpl :: forall a. (a -> String) -> Array a -> String
foreign import cons :: forall a. a -> Array a -> Array a
foreign import join :: String -> Array String -> String
