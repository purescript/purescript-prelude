module Prelude
  ( Unit(..), unit
  , ($), (#)
  , flip
  , const
  , asTypeOf
  , otherwise
  , (:), cons
  , Semigroupoid, (<<<), (>>>)
  , Category, id
  , Functor, (<$>), (<#>), void
  , Apply, (<*>)
  , Applicative, pure, liftA1
  , Bind, (>>=)
  , Monad, return, liftM1, ap
  , Semigroup, (<>), (++)
  , Semiring, (+), zero, (*), one
  , ModuloSemiring, (/), mod
  , Ring, (-), negate
  , Num
  , DivisionRing
  , Eq, (==), (/=)
  , Ordering(..), Ord, compare, (<), (>), (<=), (>=)
  , Bounded, top, bottom
  , Lattice, sup, inf, (||), (&&)
  , BoundedLattice
  , ComplementedLattice, not
  , DistributiveLattice
  , BooleanAlgebra
  , Show, show
  ) where

-- | The `Unit` type has a single inhabitant, called `unit`. It represents
-- | values with no computational content.
-- |
-- | `Unit` is often used, wrapped in a monadic type constructor, as the
-- | return type of a computation where only
-- | the _effects_ are important.
newtype Unit = Unit {}

-- | `unit` is the sole inhabitant of the `Unit` type.
unit :: Unit
unit = Unit {}

infixr 0 $
infixl 0 #

-- | Applies a function to its argument.
-- |
-- | ```purescript
-- | length $ groupBy productCategory $ filter isInStock $ products
-- | ```
-- |
-- | is equivalent to:
-- |
-- | ```purescript
-- | length (groupBy productCategory (filter isInStock products))
-- | ```
-- |
-- | `($)` is different from [`(#)`](#-2) because it is right-infix instead of
-- | left: `a $ b $ c $ d x = a $ (b $ (c $ (d $ x))) = a (b (c (d x)))`
($) :: forall a b. (a -> b) -> a -> b
($) f x = f x

-- | Applies an argument to a function.
-- |
-- | ```purescript
-- | products # filter isInStock # groupBy productCategory # length
-- | ```
-- |
-- | is equivalent to:
-- |
-- | ```purescript
-- | length (groupBy productCategory (filter isInStock products))
-- | ```
-- |
-- | `(#)` is different from [`($)`](#-1) because it is left-infix instead of
-- | right: `x # a # b # c # d = (((x # a) # b) # c) # d = d (c (b (a x)))`
(#) :: forall a b. a -> (a -> b) -> b
(#) x f = f x

-- | Flips the order of the arguments to a function of two arguments.
-- |
-- | ```purescript
-- | flip const 1 2 = const 2 1 = 2
-- | ```
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- | Returns its first argument and ignores its second.
-- |
-- | ```purescript
-- | const 1 "hello" = 1
-- | ```
const :: forall a b. a -> b -> a
const a _ = a

-- | This function returns its first argument, and can be used to assert type
-- | equalities. This can be useful when types are otherwise ambiguous.
-- |
-- | ```purescript
-- | main = print $ [] `asTypeOf` [0]
-- | ```
-- |
-- | If instead, we had written `main = print []`, the type of the argument
-- | `[]` would have been ambiguous, resulting in a compile-time error.
asTypeOf :: forall a. a -> a -> a
asTypeOf x _ = x

-- | An alias for `true`, which can be useful in guard clauses:
-- |
-- | ```purescript
-- | max x y | x >= y    = x
-- |         | otherwise = y
-- | ```
otherwise :: Boolean
otherwise = true

-- | Attaches an element to the front of an array, creating a new array.
-- |
-- | ```purescript
-- | cons 1 [2, 3, 4] = [1, 2, 3, 4]
-- | ```
-- |
-- | Note, the running time of this function is `O(n)`.
foreign import cons
  """
  function cons(e) {
    return function(l) {
      return [e].concat(l);
    };
  }
  """ :: forall a. a -> [a] -> [a]

infixr 6 :

-- | An infix alias for `cons`.
-- |
-- | Note, the running time of this function is `O(n)`.
(:) :: forall a. a -> [a] -> [a]
(:) = cons

infixr 9 >>>
infixr 9 <<<

-- | A `Semigroupoid` is similar to a [`Category`](#category) but does not
-- | require an identity element `id`, just composable morphisms.
-- |
-- | `Semigroupoid`s must satisfy the following law:
-- |
-- | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
-- |
-- | One example of a `Semigroupoid` is the function type constructor `(->)`,
-- | with `(<<<)` defined as function composition.
class Semigroupoid a where
  (<<<) :: forall b c d. a c d -> a b c -> a b d

instance semigroupoidArr :: Semigroupoid (->) where
  (<<<) f g x = f (g x)

-- | Forwards composition, or `(<<<)` with its arguments reversed.
(>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
(>>>) f g = g <<< f

-- | `Category`s consist of objects and composable morphisms between them, and
-- | as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
-- | must have an identity element.
-- |
-- | Instances must satisfy the following law in addition to the
-- | `Semigroupoid` law:
-- |
-- | - Identity: `id <<< p = p <<< id = p`
class (Semigroupoid a) <= Category a where
  id :: forall t. a t t

instance categoryArr :: Category (->) where
  id x = x

infixl 4 <$>
infixl 1 <#>

-- | A `Functor` is a type constructor which supports a mapping operation
-- | `(<$>)`.
-- |
-- | `(<$>)` can be used to turn functions `a -> b` into functions
-- | `f a -> f b` whose argument and return types use the type constructor `f`
-- | to represent some computational context.
-- |
-- | Instances must satisfy the following laws:
-- |
-- | - Identity: `(<$>) id = id`
-- | - Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`
class Functor f where
  (<$>) :: forall a b. (a -> b) -> f a -> f b

instance functorArr :: Functor ((->) r) where
  (<$>) = (<<<)

-- | `(<#>)` is `(<$>)` with its arguments reversed. For example:
-- |
-- | ```purescript
-- | [1, 2, 3] <#> \n -> n * n
-- | ```
(<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
(<#>) fa f = f <$> fa

-- | The `void` function is used to ignore the type wrapped by a
-- | [`Functor`](#functor), replacing it with `Unit` and keeping only the type
-- | information provided by the type constructor itself.
-- |
-- | `void` is often useful when using `do` notation to change the return type
-- | of a monadic computation:
-- |
-- | ```purescript
-- | main = forE 1 10 \n -> void do
-- |   print n
-- |   print (n * n)
-- | ```
void :: forall f a. (Functor f) => f a -> f Unit
void fa = const unit <$> fa

infixl 4 <*>

-- | The `Apply` class provides the `(<*>)` which is used to apply a function
-- | to an argument under a type constructor.
-- |
-- | `Apply` can be used to lift functions of two or more arguments to work on
-- | values wrapped with the type constructor `f`. It might also be understood
-- | in terms of the `lift2` function:
-- |
-- | ```purescript
-- | lift2 :: forall f a b c. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
-- | lift2 f a b = f <$> a <*> b
-- | ```
-- |
-- | `(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
-- | the function application operator `($)` to arguments wrapped with the
-- | type constructor `f`.
-- |
-- | Instances must satisfy the following law in addition to the `Functor`
-- | laws:
-- |
-- | - Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`
-- |
-- | Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
class (Functor f) <= Apply f where
  (<*>) :: forall a b. f (a -> b) -> f a -> f b

instance applyArr :: Apply ((->) r) where
  (<*>) f g x = f x (g x)

-- | The `Applicative` type class extends the [`Apply`](#apply) type class
-- | with a `pure` function, which can be used to create values of type `f a`
-- | from values of type `a`.
-- |
-- | Where [`Apply`](#apply) provides the ability to lift functions of two or
-- | more arguments to functions whose arguments are wrapped using `f`, and
-- | [`Functor`](#functor) provides the ability to lift functions of one
-- | argument, `pure` can be seen as the function which lifts functions of
-- | _zero_ arguments. That is, `Applicative` functors support a lifting
-- | operation for any number of function arguments.
-- |
-- | Instances must satisfy the following laws in addition to the `Apply`
-- | laws:
-- |
-- | - Identity: `(pure id) <*> v = v`
-- | - Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
-- | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
-- | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a

instance applicativeArr :: Applicative ((->) r) where
  pure = const

-- | `return` is an alias for `pure`.
return :: forall m a. (Applicative m) => a -> m a
return = pure

-- | `liftA1` provides a default implementation of `(<$>)` for any
-- | [`Applicative`](#applicative) functor, without using `(<$>)` as provided
-- | by the [`Functor`](#functor)-[`Applicative`](#applicative) superclass
-- | relationship.
-- |
-- | `liftA1` can therefore be used to write [`Functor`](#functor) instances
-- | as follows:
-- |
-- | ```purescript
-- | instance functorF :: Functor F where
-- |   (<$>) = liftA1
-- | ```
liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
liftA1 f a = pure f <*> a

infixl 1 >>=

-- | The `Bind` type class extends the [`Apply`](#apply) type class with a
-- | "bind" operation `(>>=)` which composes computations in sequence, using
-- | the return value of one computation to determine the next computation.
-- |
-- | The `>>=` operator can also be expressed using `do` notation, as follows:
-- |
-- | ```purescript
-- | x >>= f = do y <- x
-- |              f y
-- | ```
-- |
-- | where the function argument of `f` is given the name `y`.
-- |
-- | Instances must satisfy the following law in addition to the `Apply`
-- | laws:
-- |
-- | - Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`
-- |
-- | Associativity tells us that we can regroup operations which use `do`
-- | notation so that we can unambiguously write, for example:
-- |
-- | ```purescript
-- | do x <- m1
-- |    y <- m2 x
-- |    m3 x y
-- | ```
class (Apply m) <= Bind m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b

instance bindArr :: Bind ((->) r) where
  (>>=) m f x = f (m x) x

-- | The `Monad` type class combines the operations of the `Bind` and
-- | `Applicative` type classes. Therefore, `Monad` instances represent type
-- | constructors which support sequential composition, and also lifting of
-- | functions of arbitrary arity.
-- |
-- | Instances must satisfy the following laws in addition to the
-- | `Applicative` and `Bind` laws:
-- |
-- | - Left Identity: `pure x >>= f = f x`
-- | - Right Identity: `x >>= pure = x`
class (Applicative m, Bind m) <= Monad m

instance monadArr :: Monad ((->) r)

-- | `liftM1` provides a default implementation of `(<$>)` for any
-- | [`Monad`](#monad), without using `(<$>)` as provided by the
-- | [`Functor`](#functor)-[`Monad`](#monad) superclass relationship.
-- |
-- | `liftM1` can therefore be used to write [`Functor`](#functor) instances
-- | as follows:
-- |
-- | ```purescript
-- | instance functorF :: Functor F where
-- |   (<$>) = liftM1
-- | ```
liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
liftM1 f a = do
  a' <- a
  return (f a')

-- | `ap` provides a default implementation of `(<*>)` for any
-- | [`Monad`](#monad), without using `(<*>)` as provided by the
-- | [`Apply`](#apply)-[`Monad`](#monad) superclass relationship.
-- |
-- | `ap` can therefore be used to write [`Apply`](#apply) instances as
-- | follows:
-- |
-- | ```purescript
-- | instance applyF :: Apply F where
-- |   (<*>) = ap
-- | ```
ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
ap f a = do
  f' <- f
  a' <- a
  return (f' a')

infixr 5 <>
infixr 5 ++

-- | The `Semigroup` type class identifies an associative operation on a type.
-- |
-- | Instances are required to satisfy the following law:
-- |
-- | - Associativity: `(x <> y) <> z = x <> (y <> z)`
-- |
-- | One example of a `Semigroup` is `String`, with `(<>)` defined as string
-- | concatenation.
class Semigroup a where
  (<>) :: a -> a -> a

-- | `(++)` is an alias for `(<>)`.
(++) :: forall s. (Semigroup s) => s -> s -> s
(++) = (<>)

instance semigroupString :: Semigroup String where
  (<>) = concatString

instance semigroupUnit :: Semigroup Unit where
  (<>) _ _ = unit

instance semigroupArr :: (Semigroup s') => Semigroup (s -> s') where
  (<>) f g = \x -> f x <> g x

instance semigroupOrdering :: Semigroup Ordering where
  (<>) LT _ = LT
  (<>) GT _ = GT
  (<>) EQ y = y

foreign import concatString
  """
  function concatString(s1) {
    return function(s2) {
      return s1 + s2;
    };
  }
  """ :: String -> String -> String

infixl 6 +
infixl 7 *

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
-- | - Annihiliation: `zero * a = a * zero = zero`
class Semiring a where
  (+)  :: a -> a -> a
  zero :: a
  (*)  :: a -> a -> a
  one  :: a

instance semiringNumber :: Semiring Number where
  (+) = numAdd
  zero = 0
  (*) = numMul
  one = 1

instance semiringUnit :: Semiring Unit where
  (+) _ _ = unit
  zero = unit
  (*) _ _ = unit
  one = unit

infixl 6 -

-- | The `Ring` class is for types that support addition, multiplication,
-- | and subtraction operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Additive inverse: `a + (-a) = (-a) + a = zero`
class (Semiring a) <= Ring a where
  (-) :: a -> a -> a

instance ringNumber :: Ring Number where
  (-) = numSub

instance ringUnit :: Ring Unit where
  (-) _ _ = unit

negate :: forall a. (Ring a) => a -> a
negate a = zero - a

infixl 7 /

-- | The `ModuloSemiring` class is for types that support addition,
-- | multiplication, division, and modulo (division remainder) operations.
-- |
-- | Instances must satisfy the following law in addition to the `Semiring`
-- | laws:
-- |
-- | - Remainder: `a / b * b + (a `mod` b) = a`
class (Semiring a) <= ModuloSemiring a where
  (/) :: a -> a -> a
  mod :: a -> a -> a

instance moduloSemiringNumber :: ModuloSemiring Number where
  (/) = numDiv
  mod _ _ = 0

instance moduloSemiringUnit :: ModuloSemiring Unit where
  (/) _ _ = unit
  mod _ _ = unit

-- | A `Ring` where every nonzero element has a multiplicative inverse.
-- |
-- | Instances must satisfy the following law in addition to the `Ring` and
-- | `ModuloSemiring` laws:
-- |
-- | - Multiplicative inverse: `(one / x) * x = one`
-- |
-- | As a consequence of this ```a `mod` b = zero``` as no divide operation
-- | will have a remainder.
class (Ring a, ModuloSemiring a) <= DivisionRing a

instance divisionRingNumber :: DivisionRing Number

instance divisionRingUnit :: DivisionRing Unit

-- | The `Num` class is for types that are commutative fields.
-- |
-- | Instances must satisfy the following law in addition to the
-- | `DivisionRing` laws:
-- |
-- | - Commutative multiplication: `a * b = b * a`
class (DivisionRing a) <= Num a

instance numNumber :: Num Number

instance numUnit :: Num Unit

foreign import numAdd
  """
  function numAdd(n1) {
    return function(n2) {
      return n1 + n2;
    };
  }
  """ :: Number -> Number -> Number

foreign import numMul
  """
  function numMul(n1) {
    return function(n2) {
      return n1 * n2;
    };
  }
  """ :: Number -> Number -> Number

foreign import numDiv
  """
  function numDiv(n1) {
    return function(n2) {
      return n1 / n2;
    };
  }
  """ :: Number -> Number -> Number

foreign import numSub
  """
  function numSub(n1) {
    return function(n2) {
      return n1 - n2;
    };
  }
  """ :: Number -> Number -> Number

infix 4 ==
infix 4 /=

-- | The `Eq` type class represents types which support decidable equality.
-- |
-- | `Eq` instances should satisfy the following laws:
-- |
-- | - Reflexivity: `x == x = true`
-- | - Symmetry: `x == y = y == x`
-- | - Transitivity: if `x == y` and `y == z` then `x == z`
-- | - Negation: `x /= y = not (x == y)`
-- |
-- | `(/=)` may be implemented in terms of `(==)`, but it might give a performance improvement to implement it separately.
class Eq a where
  (==) :: a -> a -> Boolean
  (/=) :: a -> a -> Boolean

instance eqBoolean :: Eq Boolean where
  (==) = refEq
  (/=) = refIneq

instance eqNumber :: Eq Number where
  (==) = refEq
  (/=) = refIneq

instance eqString :: Eq String where
  (==) = refEq
  (/=) = refIneq

instance eqUnit :: Eq Unit where
  (==) _ _ = true
  (/=) _ _ = false

instance eqArray :: (Eq a) => Eq [a] where
  (==) = eqArrayImpl (==)
  (/=) xs ys = not (xs == ys)

instance eqOrdering :: Eq Ordering where
  (==) LT LT = true
  (==) GT GT = true
  (==) EQ EQ = true
  (==) _  _  = false
  (/=) x y = not (x == y)

foreign import refEq
  """
  function refEq(r1) {
    return function(r2) {
      return r1 === r2;
    };
  }
  """ :: forall a. a -> a -> Boolean

foreign import refIneq
  """
  function refIneq(r1) {
    return function(r2) {
      return r1 !== r2;
    };
  }
  """ :: forall a. a -> a -> Boolean

foreign import eqArrayImpl
  """
  function eqArrayImpl(f) {
    return function(xs) {
      return function(ys) {
        if (xs.length !== ys.length) return false;
        for (var i = 0; i < xs.length; i++) {
          if (!f(xs[i])(ys[i])) return false;
        }
        return true;
      };
    };
  }
  """ :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> Boolean

-- | The `Ordering` data type represents the three possible outcomes of
-- | comparing two values:
-- |
-- | `LT` - The first value is _less than_ the second.
-- | `GT` - The first value is _greater than_ the second.
-- | `EQ` - The first value is _equal to_ or _incomparable to_ the second.
data Ordering = LT | GT | EQ

-- | The `Ord` type class represents types which support comparisons.
-- |
-- | `Ord` instances should satisfy the laws of _partially orderings_:
-- |
-- | - Reflexivity: `a <= a`
-- | - Antisymmetry: if `a <= b` and `b <= a` then `a = b`
-- | - Transitivity: if `a <= b` and `b <= c` then `a <= c`
class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering

instance ordBoolean :: Ord Boolean where
  compare false false = EQ
  compare false true  = LT
  compare true  true  = EQ
  compare true  false = GT

instance ordNumber :: Ord Number where
  compare = unsafeCompare

instance ordString :: Ord String where
  compare = unsafeCompare

instance ordUnit :: Ord Unit where
  compare _ _ = EQ

instance ordArray :: (Ord a) => Ord [a] where
  compare [] [] = EQ
  compare [] _ = LT
  compare _ [] = GT
  compare (x:xs) (y:ys) = case compare x y of
    EQ -> compare xs ys
    other -> other

instance ordOrdering :: Ord Ordering where
  compare LT LT = EQ
  compare EQ EQ = EQ
  compare GT GT = EQ
  compare LT _  = LT
  compare EQ LT = GT
  compare EQ GT = LT
  compare GT _  = GT

infixl 4 <
infixl 4 >
infixl 4 <=
infixl 4 >=

-- | Test whether one value is _strictly less than_ another.
(<) :: forall a. (Ord a) => a -> a -> Boolean
(<) a1 a2 = case a1 `compare` a2 of
  LT -> true
  _ -> false

-- | Test whether one value is _strictly greater than_ another.
(>) :: forall a. (Ord a) => a -> a -> Boolean
(>) a1 a2 = case a1 `compare` a2 of
  GT -> true
  _ -> false

-- | Test whether one value is _non-strictly less than_ another.
(<=) :: forall a. (Ord a) => a -> a -> Boolean
(<=) a1 a2 = case a1 `compare` a2 of
  GT -> false
  _ -> true

-- | Test whether one value is _non-strictly greater than_ another.
(>=) :: forall a. (Ord a) => a -> a -> Boolean
(>=) a1 a2 = case a1 `compare` a2 of
  LT -> false
  _ -> true

unsafeCompare :: forall a. a -> a -> Ordering
unsafeCompare = unsafeCompareImpl LT EQ GT

foreign import unsafeCompareImpl
  """
  function unsafeCompareImpl(lt) {
    return function(eq) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x > y ? gt : eq;
          };
        };
      };
    };
  }
  """ :: forall a. Ordering -> Ordering -> Ordering -> a -> a -> Ordering

-- | The `Bounded` type class represents types that are finite partially
-- | ordered sets.
-- |
-- | Instances should satisfy the following law in addition to the `Ord` laws:
-- |
-- | - Ordering: `bottom <= a <= top`
class (Ord a) <= Bounded a where
  top :: a
  bottom :: a

instance boundedBoolean :: Bounded Boolean where
  top = true
  bottom = false

instance boundedUnit :: Bounded Unit where
  top = unit
  bottom = unit

instance boundedOrdering :: Bounded Ordering where
  top = GT
  bottom = LT

-- | The `Lattice` type class represents types that are partially ordered
-- | sets with a supremum (`sup` or `||`) and infimum (`inf` or `&&`).
-- |
-- | Instances should satisfy the following laws in addition to the `Ord`
-- | laws:
-- |
-- | - Associativity:
-- |   - `a || (b || c) = (a || b) || c`
-- |   - `a && (b && c) = (a && b) && c`
-- | - Commutativity:
-- |   - `a || b = b || a`
-- |   - `a && b = b && a`
-- | - Absorption:
-- |   - `a || (a && b) = a`
-- |   - `a && (a || b) = a`
-- | - Idempotent:
-- |   - `a || a = a`
-- |   - `a && a = a`
class (Ord a) <= Lattice a where
  sup :: a -> a -> a
  inf :: a -> a -> a

instance latticeBoolean :: Lattice Boolean where
  sup = boolOr
  inf = boolAnd

instance latticeUnit :: Lattice Unit where
  sup _ _ = unit
  inf _ _ = unit

infixr 2 ||
infixr 3 &&

-- | The `sup` operator.
(||) :: forall a. (Lattice a) => a -> a -> a
(||) = sup

-- | The `inf` operator.
(&&) :: forall a. (Lattice a) => a -> a -> a
(&&) = inf

-- | The `BoundedLattice` type class represents types that are finite
-- | lattices.
-- |
-- | Instances should satisfy the following law in addition to the `Lattice`
-- | and `Bounded` laws:
-- |
-- | - Identity:
-- |   - `a || bottom = a`
-- |   - `a && top = a`
-- | - Annihiliation:
-- |   - `a || top = top`
-- |   - `a && bottom = bottom`
class (Bounded a, Lattice a) <= BoundedLattice a

instance boundedLatticeBoolean :: BoundedLattice Boolean

instance boundedLatticeUnit :: BoundedLattice Unit

-- | The `ComplementedLattice` type class represents types that are lattices
-- | where every member is also uniquely complemented.
-- |
-- | Instances should satisfy the following law in addition to the
-- | `BoundedLattice` laws:
-- |
-- | - Complemented:
-- |   - `not a || a == top`
-- |   - `not a && a == bottom`
-- | - Double negation:
-- |   - `not <<< not == id`
class (BoundedLattice a) <= ComplementedLattice a where
  not :: a -> a

instance complementedLatticeBoolean :: ComplementedLattice Boolean where
  not = boolNot

instance complementedLatticeUnit :: ComplementedLattice Unit where
  not _ = unit

-- | The `DistributiveLattice` type class represents types that are lattices
-- | where the `&&` and `||` distribute over each other.
-- |
-- | Instances should satisfy the following law in addition to the `Lattice`
-- | laws:
-- |
-- | - Distributivity: `x && (y || z) = (x && y) || (x && z)`
class (Lattice a) <= DistributiveLattice a

instance distributiveLatticeBoolean :: DistributiveLattice Boolean

instance distributiveLatticeUnit :: DistributiveLattice Unit

-- | The `BooleanAlgebra` type class represents types that are Boolean
-- | algebras, also known as Boolean lattices.
-- |
-- | Instances should satisfy the `ComplementedLattice` and
-- | `DistributiveLattice` laws.
class (ComplementedLattice a, DistributiveLattice a) <= BooleanAlgebra a

instance booleanAlgebraBoolean :: BooleanAlgebra Boolean

instance booleanAlgebraUnit :: BooleanAlgebra Unit

foreign import boolOr
  """
  function boolOr(b1) {
    return function(b2) {
      return b1 || b2;
    };
  }
  """ :: Boolean -> Boolean -> Boolean

foreign import boolAnd
  """
  function boolAnd(b1) {
    return function(b2) {
      return b1 && b2;
    };
  }
  """  :: Boolean -> Boolean -> Boolean

foreign import boolNot
  """
  function boolNot(b) {
    return !b;
  }
  """ :: Boolean -> Boolean

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

instance showNumber :: Show Number where
  show = showNumberImpl

instance showString :: Show String where
  show = showStringImpl

instance showUnit :: Show Unit where
  show _ = "unit"

instance showArray :: (Show a) => Show [a] where
  show = showArrayImpl show

instance showOrdering :: Show Ordering where
  show LT = "LT"
  show GT = "GT"
  show EQ = "EQ"

foreign import showNumberImpl
  """
  function showNumberImpl(n) {
    return n.toString();
  }
  """ :: Number -> String

foreign import showStringImpl
  """
  function showStringImpl(s) {
    return JSON.stringify(s);
  }
  """ :: String -> String

foreign import showArrayImpl
  """
  function showArrayImpl(f) {
    return function(xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return '[' + ss.join(',') + ']';
    };
  }
  """ :: forall a. (a -> String) -> [a] -> String
