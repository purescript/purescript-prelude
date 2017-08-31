module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), cardinality, fromEnum, pred, succ, toEnum, enumFromTo)
import Data.Generic.Rep as G
import Data.Generic.Rep.Bounded as GBounded
import Data.Generic.Rep.Enum as GEnum
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)

data List a = Nil | Cons { head :: a, tail :: List a }

cons :: forall a. a -> List a -> List a
cons head tail = Cons { head, tail }

derive instance genericList :: G.Generic (List a) _

instance eqList :: Eq a => Eq (List a) where
  eq x y = GEq.genericEq x y

instance ordList :: Ord a => Ord (List a) where
  compare x y = GOrd.genericCompare x y

instance showList :: Show a => Show (List a) where
  show x = GShow.genericShow x

data SimpleBounded = A | B | C | D
derive instance genericSimpleBounded :: G.Generic SimpleBounded _
instance eqSimpleBounded :: Eq SimpleBounded where
  eq x y = GEq.genericEq x y
instance ordSimpleBounded :: Ord SimpleBounded where
  compare x y = GOrd.genericCompare x y
instance showSimpleBounded :: Show SimpleBounded where
  show x = GShow.genericShow x
instance boundedSimpleBounded :: Bounded SimpleBounded where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumSimpleBounded :: Enum SimpleBounded where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumSimpleBounded :: BoundedEnum SimpleBounded where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

data Option a = None | Some a
derive instance genericOption :: G.Generic (Option a) _
instance eqOption :: Eq a => Eq (Option a) where
  eq x y = GEq.genericEq x y
instance ordOption :: Ord a => Ord (Option a) where
  compare x y = GOrd.genericCompare x y
instance showOption :: Show a => Show (Option a) where
  show x = GShow.genericShow x
instance boundedOption :: Bounded a => Bounded (Option a) where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumOption :: (Bounded a, Enum a) => Enum (Option a) where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumOption :: BoundedEnum a => BoundedEnum (Option a) where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

data Bit = Zero | One
derive instance genericBit :: G.Generic Bit _
instance eqBit :: Eq Bit where
  eq x y = GEq.genericEq x y
instance ordBit :: Ord Bit where
  compare x y = GOrd.genericCompare x y
instance showBit :: Show Bit where
  show x = GShow.genericShow x
instance boundedBit :: Bounded Bit where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumBit :: Enum Bit where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumBit :: BoundedEnum Bit where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum

data Pair a b = Pair a b
derive instance genericPair :: G.Generic (Pair a b) _
instance eqPair :: (Eq a, Eq b) => Eq (Pair a b) where
  eq = GEq.genericEq
instance ordPair :: (Ord a, Ord b) => Ord (Pair a b) where
  compare = GOrd.genericCompare
instance showPair :: (Show a, Show b) => Show (Pair a b) where
  show = GShow.genericShow
instance boundedPair :: (Bounded a, Bounded b) => Bounded (Pair a b) where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumPair :: (Bounded a, Enum a, Bounded b, Enum b) => Enum (Pair a b) where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumPair :: (BoundedEnum a, BoundedEnum b) => BoundedEnum (Pair a b) where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum
  

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  logShow (cons 1 (cons 2 Nil))

  log "Checking equality"
  assert $ cons 1 (cons 2 Nil) == cons 1 (cons 2 Nil)

  log "Checking inequality"
  assert $ cons 1 (cons 2 Nil) /= cons 1 Nil

  log "Checking comparison EQ"
  assert $ (cons 1 (cons 2 Nil) `compare` cons 1 (cons 2 Nil)) == EQ

  log "Checking comparison GT"
  assert $ (cons 1 (cons 2 Nil) `compare` cons 1 Nil) == GT

  log "Checking comparison LT"
  assert $ (cons 1 Nil `compare` cons 1 (cons 2 Nil)) == LT

  log "Checking simple bottom"
  assert $ bottom == A

  log "Checking simple top"
  assert $ top == D

  log "Checking composite bottom"
  assert $ bottom == None :: Option SimpleBounded

  log "Checking composite top"
  assert $ top == Some D

  log "Checking product bottom"
  assert $ bottom == Pair Zero A :: Pair Bit SimpleBounded

  log "Checking product top"
  assert $ top == Pair One D :: Pair Bit SimpleBounded

  log "Checking simple pred bottom"
  assert $ pred (bottom :: SimpleBounded) == Nothing

  log "Checking simple (pred =<< succ bottom)"
  assert $ (pred =<< succ bottom) == Just A

  log "Checking simple succ top"
  assert $ succ (top :: SimpleBounded) == Nothing

  log "Checking simple (succ =<< pred top)"
  assert $ (succ =<< pred top) == Just D

  log "Checking composite pred bottom"
  assert $ pred (bottom :: Option SimpleBounded) == Nothing

  log "Checking composite (pred =<< succ bottom)"
  assert $ (pred =<< succ (bottom :: Option SimpleBounded)) == Just None

  log "Checking composite succ top"
  assert $ succ (top :: Option SimpleBounded) == Nothing

  log "Checking composite (succ =<< pred top)"
  assert $ (succ =<< pred top) == Just (Some D)

  log "Checking product pred bottom"
  assert $ pred (bottom :: Pair Bit SimpleBounded) == Nothing

  log "Checking product (pred =<< succ bottom)"
  assert $ (pred =<< succ (bottom :: Pair Bit SimpleBounded)) == Just (Pair Zero A)

  log "Checking product succ top"
  assert $ succ (top :: Pair Bit SimpleBounded) == Nothing

  log "Checking product (succ =<< pred top)"
  assert $ (succ =<< pred top) == Just (Pair One D)

  log "Checking simple cardinality"
  assert $ (cardinality :: Cardinality SimpleBounded) == Cardinality 4

  log "Checking composite cardinality"
  assert $ (cardinality :: Cardinality (Option SimpleBounded)) == Cardinality 5

  log "Checking product cardinality"
  assert $ (cardinality :: Cardinality (Pair Bit SimpleBounded)) == Cardinality 8

  log "Checking simple toEnum/fromEnum roundtrip"
  assert $ toEnum (fromEnum A) == Just A
  assert $ toEnum (fromEnum B) == Just B

  log "Checking composite toEnum/fromEnum roundtrip"
  assert $ toEnum (fromEnum (None :: Option SimpleBounded)) == Just (None :: Option SimpleBounded)
  assert $ toEnum (fromEnum (Some A)) == Just (Some A)

  log "Checking product toEnum/fromEnum roundtrip"
  assert $ let allPairs = enumFromTo bottom top :: Array (Pair Bit SimpleBounded)
           in toEnum <<< fromEnum <$> allPairs == Just <$> allPairs
