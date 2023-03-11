module Data.Hashable (
  class Hashable,
  hashWithSalt,
  hash,

  class HashableRecord,
  hashRecord,

  Hash(Hash)
) where

import Data.Eq (class Eq, class EqRecord)
import Data.Ord (class Ord)
import Data.Ordering (Ordering(..))
import Data.Ring ((-))
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
-- | equality and a seeded hash function whose result can approximate
-- | equality for use in hash-based algorithms and data structures.
-- |
-- | Instances of `Hashable` must satisfy the following law:
-- |
-- | ```PureScript
-- | (a == b) `implies` (hashWithSalt s a == hashWithSalt s b)
-- | ```
-- |
-- | That is, unequal hash values are a safe approximation of
-- | inequality. In other words, two objects whose hash values differ,
-- | are never equal. The reverse is not necessarily true.
-- |
-- | Hash values produced by `hashWithSalt` are not cryptographically
-- | secure, must not be relied upon to be stable across multiple
-- | executions of a program, and should not be stored externally.
class Eq a <= Hashable a where
  hashWithSalt :: Int -> a -> Hash a

-- | The `Hash a` newtype wraps the hash code of a value of type `a`.
-- |
-- | Hash values should not be stored externally, as they must not be
-- | relied upon to be stable across multiple executions of a program.
newtype Hash a = Hash Int

-- | A convenience function that calls `hashWithSalt` on its argument
-- | with some seed. This seed is fixed for the runtime of a program,
-- | but may change between runs. Do not store hashes externally.
hash :: forall a. Hashable a => a -> Hash a
hash = hashWithSalt 42

instance showHash :: Show (Hash a) where
  show (Hash n) = "(Hash " <> show n <> ")"
derive newtype instance eqHash :: Eq (Hash a)
derive newtype instance ordHash :: Ord (Hash a)

instance hashableBoolean :: Hashable Boolean where
  hashWithSalt s b = Hash (s + if b then 1 else 0)

instance hashableInt :: Hashable Int where
  hashWithSalt s n = Hash (s + n)

foreign import hashNumber :: Int -> Number -> Hash Number

instance hashableNumber :: Hashable Number where
  hashWithSalt = hashNumber

foreign import hashChar :: Int -> Char -> Hash Char

instance hashableChar :: Hashable Char where
  hashWithSalt = hashChar

foreign import hashString :: Int -> String -> Hash String

instance hashableString :: Hashable String where
  hashWithSalt = hashString

foreign import hashArray :: forall a. (a -> Hash a) -> Int -> Array a -> Hash (Array a)

instance hashableArray :: Hashable a => Hashable (Array a) where
  hashWithSalt = hashArray hash

instance hashableUnit :: Hashable Unit where
  hashWithSalt s _ = Hash (s + 1)

instance hashableVoid :: Hashable Void where
  hashWithSalt s _ = Hash s

instance hashableOrdering :: Hashable Ordering where
  hashWithSalt s LT = Hash (s - 1)
  hashWithSalt s GT = Hash (s + 1)
  hashWithSalt s EQ = Hash (s + 0)

class EqRecord l r <= HashableRecord l r | l -> r where
  hashRecord :: Int -> RLProxy l -> Record r -> Hash (Record r)

instance hashableRecordNil :: HashableRecord Nil r where
  hashRecord s _ _ = Hash s

instance hashableRecordCons ::
  ( Hashable vt
  , HashableRecord tl r
  , IsSymbol l
  , Row.Cons l vt whatev r
  ) => HashableRecord (Cons l vt tl) r where
  hashRecord s rlp record =
    let (Hash rHash) = hashRecord s (RLProxy :: RLProxy tl) record
        field :: vt
        field = unsafeGet (reflectSymbol (SProxy :: SProxy l)) record
        (Hash fHash) = hashWithSalt s field
    in Hash (rHash * 31 + fHash)

instance hashableRecord ::
  (RowToList r l, HashableRecord l r, EqRecord l r)
  => Hashable (Record r) where
  hashWithSalt s = hashRecord s (RLProxy :: RLProxy l)
