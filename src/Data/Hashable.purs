module Data.Hashable (
  class Hashable,
  hash,

  class HashableRecord,
  hashRecord,

  Hash(Hash)
) where

import Data.Eq (class Eq, class EqRecord)
import Data.Ord (class Ord)
import Data.Ordering (Ordering(..))
import Data.Ring (negate)
import Data.Semigroup ((<>))
import Data.Semiring ((*), (+))
import Data.Show (class Show, show)
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
-- | Hash values produced by `hash` must not be relied upon to be
-- | stable across multiple executions of a program and should not be
-- | stored externally.
class Eq a <= Hashable a where
  hash :: a -> Hash a

-- | The `Hash a` newtype wraps the hash code of a value of type `a`.
-- |
-- | Hash values should not be stored externally, as they must not be
-- | relied upon to be stable across multiple executions of a
-- | program.
newtype Hash a = Hash Int

instance showHash :: Show (Hash a) where
  show (Hash n) = "(Hash " <> show n <> ")"
derive newtype instance eqHash :: Eq (Hash a)
derive newtype instance ordHash :: Ord (Hash a)

instance hashableBoolean :: Hashable Boolean where
  hash b = if b then Hash 1 else Hash 0

instance hashableInt :: Hashable Int where
  hash n = Hash n

foreign import hashNumber :: Number -> Hash Number

instance hashableNumber :: Hashable Number where
  hash = hashNumber

foreign import hashChar :: Char -> Hash Char

instance hashableChar :: Hashable Char where
  hash = hashChar

foreign import hashString :: String -> Hash String

instance hashableString :: Hashable String where
  hash = hashString

foreign import hashArray :: forall a. (a -> Hash a) -> Array a -> Hash (Array a)

instance hashableArray :: Hashable a => Hashable (Array a) where
  hash = hashArray hash

instance hashableUnit :: Hashable Unit where
  hash _ = Hash 1

instance hashableVoid :: Hashable Void where
  hash _ = Hash 0

instance hashableOrdering :: Hashable Ordering where
  hash LT = Hash (-1)
  hash GT = Hash 1
  hash EQ = Hash 0

class EqRecord l r <= HashableRecord l r | l -> r where
  hashRecord :: RLProxy l -> Record r -> Hash (Record r)

instance hashableRecordNil :: HashableRecord Nil r where
  hashRecord _ _ = Hash 0

instance hashableRecordCons ::
  ( Hashable vt
  , HashableRecord tl r
  , IsSymbol l
  , Row.Cons l vt whatev r
  ) => HashableRecord (Cons l vt tl) r where
  hashRecord rlp record =
    let (Hash rHash) = hashRecord (RLProxy :: RLProxy tl) record
        field :: vt
        field = unsafeGet (reflectSymbol (SProxy :: SProxy l)) record
        (Hash fHash) = hash field
    -- this mimics Java's hash function for arrays
    in Hash (rHash * 31 + fHash)

instance hashableRecord ::
  (RowToList r l, HashableRecord l r, EqRecord l r)
  => Hashable (Record r) where
  hash = hashRecord (RLProxy :: RLProxy l)
