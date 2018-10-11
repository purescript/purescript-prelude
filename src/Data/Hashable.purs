module Data.Hashable (
  class Hashable,
  hash,

  class HashableRecord,
  hashRecord
) where

import Data.Eq (class Eq, class EqRecord)
import Data.Semiring ((*), (+))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit)
import Data.Void (Void)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Unsafe (unsafeGet)
import Type.Data.RowList (RLProxy(..))

-- | The `Hashable` type class represents types with decidable
-- | equality and a hash function whose result can approximate
-- | equality for use in hash-based algorithms and data structures.
-- |
-- | Instances of `Hashable` must satisfy the following law:
-- |
-- | ```PureScript
-- | (a == b) `implies` (hash a == hash b)
-- | ```
-- | 
-- | That is, unequal hash values are a safe approximation of
-- | inequality. In other words, two objects whose hash values differ,
-- | are never equal. The reverse is not necessarily true.
-- |
-- | Hash values produced by `hash` should not be relied upon to be
-- | stable accross multiple executions of a program and should not be
-- | stored externally.
class Eq a <= Hashable a where
  hash :: a -> Int

instance hashableBoolean :: Hashable Boolean where
  hash b = if b then 1 else 0

instance hashableInt :: Hashable Int where
  hash n = n

foreign import hashNumber :: Number -> Int

instance hashableNumber :: Hashable Number where
  hash = hashNumber

foreign import hashChar :: Char -> Int

instance hashableChar :: Hashable Char where
  hash = hashChar

foreign import hashString :: String -> Int

instance hashableString :: Hashable String where
  hash = hashString

foreign import hashArray :: forall a. (a -> Int) -> Array a -> Int

instance hashableArray :: Hashable a => Hashable (Array a) where
  hash = hashArray hash

instance hashableUnit :: Hashable Unit where
  hash _ = 1

instance hashableVoid :: Hashable Void where
  hash _ = 0

class EqRecord l r <= HashableRecord l r | l -> r where
  hashRecord :: RLProxy l -> Record r -> Int

instance hashableRecordNil :: HashableRecord Nil r where
  hashRecord _ _ = 0

instance hashableRecordCons ::
  ( Hashable vt
  , HashableRecord tl r
  , IsSymbol l
  , Row.Cons l vt whatev r
  ) => HashableRecord (Cons l vt tl) r where
  hashRecord rlp record =
    hash field * 31 + hashRecord (RLProxy :: RLProxy tl) record
    where
      field :: vt
      field = unsafeGet (reflectSymbol (SProxy :: SProxy l)) record

instance hashableRecord ::
  (RowToList r l, HashableRecord l r, EqRecord l r)
  => Hashable (Record r) where
  hash = hashRecord (RLProxy :: RLProxy l)
