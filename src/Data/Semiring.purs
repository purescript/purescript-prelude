module Data.Semiring
  ( class Semiring
  , add
  , (+)
  , zero
  , mul
  , (*)
  , one

  , class SemiringRecord
  , addRecordImpl
  , mulRecordImpl
  , oneRecordImpl
  , zeroRecordImpl
  ) where

import Data.Internal.Record (unsafeGet, unsafeInsert)
import Data.RowList (RLProxy(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Data.Row (RProxy(..))
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL

-- | The `Semiring` class is for types that support an addition and
-- | multiplication operation.
-- |
-- | Instances must satisfy the following laws:
-- |
-- | - Commutative monoid under addition:
-- |   - Associativity: `(a + b) + c = a + (b + c)`
-- |   - Identity: `zero + a = a + zero = a`
-- |   - Commutative: `a + b = b + a`
-- | - Monoid under multiplication:
-- |   - Associativity: `(a * b) * c = a * (b * c)`
-- |   - Identity: `one * a = a * one = a`
-- | - Multiplication distributes over addition:
-- |   - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
-- |   - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
-- | - Annihilation: `zero * a = a * zero = zero`
-- |
-- | **Note:** The `Number` and `Int` types are not fully law abiding
-- | members of this class hierarchy due to the potential for arithmetic
-- | overflows, and in the case of `Number`, the presence of `NaN` and
-- | `Infinity` values. The behaviour is unspecified in these cases.
class Semiring a where
  add  :: a -> a -> a
  zero :: a
  mul  :: a -> a -> a
  one  :: a

infixl 6 add as +
infixl 7 mul as *

instance semiringInt :: Semiring Int where
  add = intAdd
  zero = 0
  mul = intMul
  one = 1

instance semiringNumber :: Semiring Number where
  add = numAdd
  zero = 0.0
  mul = numMul
  one = 1.0

instance semiringFn :: Semiring b => Semiring (a -> b) where
  add f g x = f x + g x
  zero = \_ -> zero
  mul f g x = f x * g x
  one = \_ -> one

instance semiringUnit :: Semiring Unit where
  add _ _ = unit
  zero = unit
  mul _ _ = unit
  one = unit

foreign import intAdd :: Int -> Int -> Int
foreign import intMul :: Int -> Int -> Int
foreign import numAdd :: Number -> Number -> Number
foreign import numMul :: Number -> Number -> Number

class SemiringRecord rowlist row subrow focus | rowlist -> subrow focus where
  addRecordImpl  :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  mulRecordImpl  :: RLProxy rowlist -> Record row -> Record row -> Record subrow
  oneRecordImpl  :: RLProxy rowlist -> RProxy row -> Record subrow
  zeroRecordImpl :: RLProxy rowlist -> RProxy row -> Record subrow

instance semiringRecordNil :: SemiringRecord RL.Nil row () focus where
  addRecordImpl  _ _ _ = {}
  mulRecordImpl  _ _ _ = {}
  oneRecordImpl  _ _   = {}
  zeroRecordImpl _ _   = {}

instance semiringRecordCons
    :: ( IsSymbol key
       , Row.Cons key focus subrowTail subrow
       , SemiringRecord rowlistTail row subrowTail subfocus
       , Semiring focus
       )
    => SemiringRecord (RL.Cons key focus rowlistTail) row subrow focus where
  addRecordImpl _ ra rb
    = unsafeInsert key
        (unsafeGet' key ra + unsafeGet' key rb)
        (addRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb)
    where key = reflectSymbol (SProxy :: SProxy key)
          unsafeGet' = unsafeGet :: String -> Record row -> focus

  mulRecordImpl _ ra rb
    = unsafeInsert key
        (unsafeGet' key ra * unsafeGet' key rb)
        (mulRecordImpl (RLProxy :: RLProxy rowlistTail) ra rb)
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus

  oneRecordImpl _ _
    = unsafeInsert key (one :: focus)
        ( oneRecordImpl
            (RLProxy :: RLProxy rowlistTail)
            (RProxy :: RProxy row)
        )
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus

  zeroRecordImpl _ _
    = unsafeInsert key (one :: focus)
        ( zeroRecordImpl
            (RLProxy :: RLProxy rowlistTail)
            (RProxy :: RProxy row)
        )
    where
      key = reflectSymbol (SProxy :: SProxy key)
      unsafeGet' = unsafeGet :: String -> Record row -> focus

instance semiringRecord
    :: ( RL.RowToList row list
       , SemiringRecord list row row focus
       )
    => Semiring (Record row) where
  add = addRecordImpl  (RLProxy :: RLProxy list)
  mul = mulRecordImpl  (RLProxy :: RLProxy list)
  one = oneRecordImpl  (RLProxy :: RLProxy list) (RProxy :: RProxy row)
  zero = zeroRecordImpl (RLProxy :: RLProxy list) (RProxy :: RProxy row)
