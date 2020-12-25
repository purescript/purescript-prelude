module Data.Generic.UsingGeneric where

import Data.Generic.Rep (class Generic)
import Data.Eq (class Eq)
import Data.Eq.Generic (genericEq)
import Data.Ord (class Ord)
import Data.Ord.Generic (genericCompare)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Semigroup (class Semigroup)
import Data.Semigroup.Generic (genericAppend)
import Data.Monoid (class Monoid)
import Data.Monoid.Generic (genericMempty)
import Data.Semiring (class Semiring)
import Data.Semiring.Generic (genericAdd, genericZero, genericMul, genericOne)
import Data.HeytingAlgebra (class HeytingAlgebra)
import Data.HeytingAlgebra.Generic (genericTT, genericFF, genericImplies, genericConj, genericDisj, genericNot)
import Data.Ring (class Ring)
import Data.Ring.Generic (genericSub)
import Data.Bounded (class Bounded)
import Data.Bounded.Generic (genericBottom, genericTop)

newtype UsingGeneric a = UsingGeneric a

instance eqUsingGeneric :: Generic a rep => Eq (UsingGeneric a) where
  eq (UsingGeneric l) (UsingGeneric r) = genericEq l r

instance ordUsingGeneric :: Generic a rep => Ord (UsingGeneric a) where
  compare (UsingGeneric  l) (UsingGeneric  r) = genericCompare l r

instance boundedUsingGeneric :: Generic a rep => Bounded (UsingGeneric a) where
  bottom = UsingGeneric genericBottom
  top = UsingGeneric genericTop

instance showUsingGeneric :: Generic a rep => Show (UsingGeneric a) where
  show (UsingGeneric x) = genericShow x

instance semigroupUsingGeneric :: Generic a rep => Semigroup (UsingGeneric a) where
  append (UsingGeneric l) (UsingGeneric r) = UsingGeneric (genericAppend l r)

instance monoidUsingGeneric :: Generic a rep => Monoid (UsingGeneric a) where
  mempty = UsingGeneric genericMempty

instance semiringUsingGeneric :: Generic a rep => Semiring (UsingGeneric a) where
  add (UsingGeneric l) (UsingGeneric r) = UsingGeneric (genericAdd l r)
  zero = UsingGeneric genericZero
  mul (UsingGeneric l) (UsingGeneric r) = UsingGeneric (genericMul l r)
  one = UsingGeneric genericOne

instance ringUsingGeneric :: Generic a rep => Ring (UsingGeneric a) where
  sub (UsingGeneric l) (UsingGeneric r) = UsingGeneric (genericSub l r)

instance heytingAlgebraUsingGeneric :: Generic a rep => HeytingAlgebra (UsingGeneric a) where
  ff = UsingGeneric genericFF
  tt = UsingGeneric genericTT
  implies (UsingGeneric x) (UsingGeneric y) = UsingGeneric (genericImplies x y)
  conj (UsingGeneric x) (UsingGeneric y) = UsingGeneric (genericConj x y)
  disj (UsingGeneric x) (UsingGeneric y) = UsingGeneric (genericDisj x y)
  not (UsingGeneric x) = UsingGeneric (genericNot x)
