## Module Prelude

#### `Unit`

``` purescript
newtype Unit
```

##### Instances
``` purescript
instance semigroupUnit :: Semigroup Unit
instance semiringUnit :: Semiring Unit
instance ringUnit :: Ring Unit
instance moduloSemiringUnit :: ModuloSemiring Unit
instance divisionRingUnit :: DivisionRing Unit
instance numUnit :: Num Unit
instance eqUnit :: Eq Unit
instance ordUnit :: Ord Unit
instance boundedUnit :: Bounded Unit
instance latticeUnit :: Lattice Unit
instance boundedLatticeUnit :: BoundedLattice Unit
instance complementedLatticeUnit :: ComplementedLattice Unit
instance distributiveLatticeUnit :: DistributiveLattice Unit
instance booleanAlgebraUnit :: BooleanAlgebra Unit
instance showUnit :: Show Unit
```

The `Unit` type has a single inhabitant, called `unit`. It represents
values with no computational content.

`Unit` is often used, wrapped in a monadic type constructor, as the
return type of a computation where only
the _effects_ are important.

#### `unit`

``` purescript
unit :: Unit
```

`unit` is the sole inhabitant of the `Unit` type.

#### `($)`

``` purescript
($) :: forall a b. (a -> b) -> a -> b
```

Applies a function to its argument.

```purescript
length $ groupBy productCategory $ filter isInStock $ products
```

is equivalent to:

```purescript
length (groupBy productCategory (filter isInStock products))
```

`($)` is different from [`(#)`](#-2) because it is right-infix instead of
left: `a $ b $ c $ d x = a $ (b $ (c $ (d $ x))) = a (b (c (d x)))`

#### `(#)`

``` purescript
(#) :: forall a b. a -> (a -> b) -> b
```

Applies an argument to a function.

```purescript
products # filter isInStock # groupBy productCategory # length
```

is equivalent to:

```purescript
length (groupBy productCategory (filter isInStock products))
```

`(#)` is different from [`($)`](#-1) because it is left-infix instead of
right: `x # a # b # c # d = (((x # a) # b) # c) # d = d (c (b (a x)))`

#### `flip`

``` purescript
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
```

Flips the order of the arguments to a function of two arguments.

```purescript
flip const 1 2 = const 2 1 = 2
```

#### `const`

``` purescript
const :: forall a b. a -> b -> a
```

Returns its first argument and ignores its second.

```purescript
const 1 "hello" = 1
```

#### `asTypeOf`

``` purescript
asTypeOf :: forall a. a -> a -> a
```

This function returns its first argument, and can be used to assert type
equalities. This can be useful when types are otherwise ambiguous.

```purescript
main = print $ [] `asTypeOf` [0]
```

If instead, we had written `main = print []`, the type of the argument
`[]` would have been ambiguous, resulting in a compile-time error.

#### `otherwise`

``` purescript
otherwise :: Boolean
```

An alias for `true`, which can be useful in guard clauses:

```purescript
max x y | x >= y    = x
        | otherwise = y
```

#### `Semigroupoid`

``` purescript
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d
```

##### Instances
``` purescript
instance semigroupoidFn :: Semigroupoid Function
```

A `Semigroupoid` is similar to a [`Category`](#category) but does not
require an identity element `id`, just composable morphisms.

`Semigroupoid`s must satisfy the following law:

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`

One example of a `Semigroupoid` is the function type constructor `(->)`,
with `(<<<)` defined as function composition.

#### `(<<<)`

``` purescript
(<<<) :: forall a b c d. (Semigroupoid a) => a c d -> a b c -> a b d
```

#### `(>>>)`

``` purescript
(>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
```

Forwards composition, or `(<<<)` with its arguments reversed.

#### `Category`

``` purescript
class (Semigroupoid a) <= Category a where
  id :: forall t. a t t
```

##### Instances
``` purescript
instance categoryFn :: Category Function
```

`Category`s consist of objects and composable morphisms between them, and
as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
must have an identity element.

Instances must satisfy the following law in addition to the
`Semigroupoid` law:

- Identity: `id <<< p = p <<< id = p`

#### `Functor`

``` purescript
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

##### Instances
``` purescript
instance functorFn :: Functor (Function r)
instance functorArray :: Functor Array
```

A `Functor` is a type constructor which supports a mapping operation
`(<$>)`.

`(<$>)` can be used to turn functions `a -> b` into functions
`f a -> f b` whose argument and return types use the type constructor `f`
to represent some computational context.

Instances must satisfy the following laws:

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`

#### `(<$>)`

``` purescript
(<$>) :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
```

#### `(<#>)`

``` purescript
(<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
```

`(<#>)` is `(<$>)` with its arguments reversed. For example:

```purescript
[1, 2, 3] <#> \n -> n * n
```

#### `void`

``` purescript
void :: forall f a. (Functor f) => f a -> f Unit
```

The `void` function is used to ignore the type wrapped by a
[`Functor`](#functor), replacing it with `Unit` and keeping only the type
information provided by the type constructor itself.

`void` is often useful when using `do` notation to change the return type
of a monadic computation:

```purescript
main = forE 1 10 \n -> void do
  print n
  print (n * n)
```

#### `Apply`

``` purescript
class (Functor f) <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
```

##### Instances
``` purescript
instance applyFn :: Apply (Function r)
instance applyArray :: Apply Array
```

The `Apply` class provides the `(<*>)` which is used to apply a function
to an argument under a type constructor.

`Apply` can be used to lift functions of two or more arguments to work on
values wrapped with the type constructor `f`. It might also be understood
in terms of the `lift2` function:

```purescript
lift2 :: forall f a b c. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b
```

`(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts
the function application operator `($)` to arguments wrapped with the
type constructor `f`.

Instances must satisfy the following law in addition to the `Functor`
laws:

- Associative composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`

Formally, `Apply` represents a strong lax semi-monoidal endofunctor.

#### `(<*>)`

``` purescript
(<*>) :: forall f a b. (Apply f) => f (a -> b) -> f a -> f b
```

#### `Applicative`

``` purescript
class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a
```

##### Instances
``` purescript
instance applicativeFn :: Applicative (Function r)
instance applicativeArray :: Applicative Array
```

The `Applicative` type class extends the [`Apply`](#apply) type class
with a `pure` function, which can be used to create values of type `f a`
from values of type `a`.

Where [`Apply`](#apply) provides the ability to lift functions of two or
more arguments to functions whose arguments are wrapped using `f`, and
[`Functor`](#functor) provides the ability to lift functions of one
argument, `pure` can be seen as the function which lifts functions of
_zero_ arguments. That is, `Applicative` functors support a lifting
operation for any number of function arguments.

Instances must satisfy the following laws in addition to the `Apply`
laws:

- Identity: `(pure id) <*> v = v`
- Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
- Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
- Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`

#### `return`

``` purescript
return :: forall m a. (Applicative m) => a -> m a
```

`return` is an alias for `pure`.

#### `liftA1`

``` purescript
liftA1 :: forall f a b. (Applicative f) => (a -> b) -> f a -> f b
```

`liftA1` provides a default implementation of `(<$>)` for any
[`Applicative`](#applicative) functor, without using `(<$>)` as provided
by the [`Functor`](#functor)-[`Applicative`](#applicative) superclass
relationship.

`liftA1` can therefore be used to write [`Functor`](#functor) instances
as follows:

```purescript
instance functorF :: Functor F where
  map = liftA1
```

#### `Bind`

``` purescript
class (Apply m) <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b
```

##### Instances
``` purescript
instance bindFn :: Bind (Function r)
instance bindArray :: Bind Array
```

The `Bind` type class extends the [`Apply`](#apply) type class with a
"bind" operation `(>>=)` which composes computations in sequence, using
the return value of one computation to determine the next computation.

The `>>=` operator can also be expressed using `do` notation, as follows:

```purescript
x >>= f = do y <- x
             f y
```

where the function argument of `f` is given the name `y`.

Instances must satisfy the following law in addition to the `Apply`
laws:

- Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`

Associativity tells us that we can regroup operations which use `do`
notation so that we can unambiguously write, for example:

```purescript
do x <- m1
   y <- m2 x
   m3 x y
```

#### `(>>=)`

``` purescript
(>>=) :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
```

#### `Monad`

``` purescript
class (Applicative m, Bind m) <= Monad m
```

##### Instances
``` purescript
instance monadFn :: Monad (Function r)
instance monadArray :: Monad Array
```

The `Monad` type class combines the operations of the `Bind` and
`Applicative` type classes. Therefore, `Monad` instances represent type
constructors which support sequential composition, and also lifting of
functions of arbitrary arity.

Instances must satisfy the following laws in addition to the
`Applicative` and `Bind` laws:

- Left Identity: `pure x >>= f = f x`
- Right Identity: `x >>= pure = x`

#### `liftM1`

``` purescript
liftM1 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
```

`liftM1` provides a default implementation of `(<$>)` for any
[`Monad`](#monad), without using `(<$>)` as provided by the
[`Functor`](#functor)-[`Monad`](#monad) superclass relationship.

`liftM1` can therefore be used to write [`Functor`](#functor) instances
as follows:

```purescript
instance functorF :: Functor F where
  map = liftM1
```

#### `ap`

``` purescript
ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
```

`ap` provides a default implementation of `(<*>)` for any
[`Monad`](#monad), without using `(<*>)` as provided by the
[`Apply`](#apply)-[`Monad`](#monad) superclass relationship.

`ap` can therefore be used to write [`Apply`](#apply) instances as
follows:

```purescript
instance applyF :: Apply F where
  apply = ap
```

#### `Semigroup`

``` purescript
class Semigroup a where
  append :: a -> a -> a
```

##### Instances
``` purescript
instance semigroupString :: Semigroup String
instance semigroupUnit :: Semigroup Unit
instance semigroupFn :: (Semigroup s') => Semigroup (s -> s')
instance semigroupOrdering :: Semigroup Ordering
instance semigroupArray :: Semigroup (Array a)
```

The `Semigroup` type class identifies an associative operation on a type.

Instances are required to satisfy the following law:

- Associativity: `(x <> y) <> z = x <> (y <> z)`

One example of a `Semigroup` is `String`, with `(<>)` defined as string
concatenation.

#### `(<>)`

``` purescript
(<>) :: forall s. (Semigroup s) => s -> s -> s
```

`(<>)` is an alias for `append`.

#### `(++)`

``` purescript
(++) :: forall s. (Semigroup s) => s -> s -> s
```

`(++)` is an alias for `append`.

#### `Semiring`

``` purescript
class Semiring a where
  add :: a -> a -> a
  zero :: a
  mul :: a -> a -> a
  one :: a
```

##### Instances
``` purescript
instance semiringInt :: Semiring Int
instance semiringNumber :: Semiring Number
instance semiringUnit :: Semiring Unit
```

The `Semiring` class is for types that support an addition and
multiplication operation.

Instances must satisfy the following laws:

- Commutative monoid under addition:
  - Associativity: `(a + b) + c = a + (b + c)`
  - Identity: `zero + a = a + zero = a`
  - Commutative: `a + b = b + a`
- Monoid under multiplication:
  - Associativity: `(a * b) * c = a * (b * c)`
  - Identity: `one * a = a * one = a`
- Multiplication distributes over addition:
  - Left distributivity: `a * (b + c) = (a * b) + (a * c)`
  - Right distributivity: `(a + b) * c = (a * c) + (b * c)`
- Annihiliation: `zero * a = a * zero = zero`

#### `(+)`

``` purescript
(+) :: forall a. (Semiring a) => a -> a -> a
```

`(+)` is an alias for `add`.

#### `(*)`

``` purescript
(*) :: forall a. (Semiring a) => a -> a -> a
```

`(*)` is an alias for `mul`.

#### `succ`

``` purescript
succ :: forall a. (Semiring a) => a -> a
```

Adds `one` to a value.

#### `Ring`

``` purescript
class (Semiring a) <= Ring a where
  sub :: a -> a -> a
```

##### Instances
``` purescript
instance ringInt :: Ring Int
instance ringNumber :: Ring Number
instance ringUnit :: Ring Unit
```

The `Ring` class is for types that support addition, multiplication,
and subtraction operations.

Instances must satisfy the following law in addition to the `Semiring`
laws:

- Additive inverse: `a - a = (zero - a) + a = zero`

#### `(-)`

``` purescript
(-) :: forall a. (Ring a) => a -> a -> a
```

`(-)` is an alias for `sub`.

#### `negate`

``` purescript
negate :: forall a. (Ring a) => a -> a
```

`negate x` can be used as a shorthand for `zero - x`.

#### `pred`

``` purescript
pred :: forall a. (Ring a) => a -> a
```

Subtracts `one` from a value.

#### `abs`

``` purescript
abs :: forall a. (Ord a, Ring a) => a -> a
```

The absolute value of a value. Defined as
`if x <= zero then negate x else x`.

#### `ModuloSemiring`

``` purescript
class (Semiring a) <= ModuloSemiring a where
  div :: a -> a -> a
  mod :: a -> a -> a
```

##### Instances
``` purescript
instance moduloSemiringInt :: ModuloSemiring Int
instance moduloSemiringNumber :: ModuloSemiring Number
instance moduloSemiringUnit :: ModuloSemiring Unit
```

The `ModuloSemiring` class is for types that support addition,
multiplication, division, and modulo (division remainder) operations.

Instances must satisfy the following law in addition to the `Semiring`
laws:

- Remainder: `a / b * b + (a `mod` b) = a`

#### `(/)`

``` purescript
(/) :: forall a. (ModuloSemiring a) => a -> a -> a
```

`(/)` is an alias for `div`.

#### `sigNum`

``` purescript
sigNum :: forall a. (Ord a, Ring a, ModuloSemiring a) => a -> a
```

The 'sign' of a value. Defined as
`if x == zero then zero else x / abs x`.

#### `DivisionRing`

``` purescript
class (Ring a, ModuloSemiring a) <= DivisionRing a
```

##### Instances
``` purescript
instance divisionRingNumber :: DivisionRing Number
instance divisionRingUnit :: DivisionRing Unit
```

A `Ring` where every nonzero element has a multiplicative inverse.

Instances must satisfy the following law in addition to the `Ring` and
`ModuloSemiring` laws:

- Multiplicative inverse: `(one / x) * x = one`

As a consequence of this ```a `mod` b = zero``` as no divide operation
will have a remainder.

#### `Num`

``` purescript
class (DivisionRing a) <= Num a
```

##### Instances
``` purescript
instance numNumber :: Num Number
instance numUnit :: Num Unit
```

The `Num` class is for types that are commutative fields.

Instances must satisfy the following law in addition to the
`DivisionRing` laws:

- Commutative multiplication: `a * b = b * a`

#### `Eq`

``` purescript
class Eq a where
  eq :: a -> a -> Boolean
```

##### Instances
``` purescript
instance eqBoolean :: Eq Boolean
instance eqInt :: Eq Int
instance eqNumber :: Eq Number
instance eqChar :: Eq Char
instance eqString :: Eq String
instance eqUnit :: Eq Unit
instance eqArray :: (Eq a) => Eq (Array a)
instance eqOrdering :: Eq Ordering
instance eqDesc :: (Eq a) => Eq (Desc a)
```

The `Eq` type class represents types which support decidable equality.

`Eq` instances should satisfy the following laws:

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`

#### `(==)`

``` purescript
(==) :: forall a. (Eq a) => a -> a -> Boolean
```

`(==)` is an alias for `eq`.

#### `(/=)`

``` purescript
(/=) :: forall a. (Eq a) => a -> a -> Boolean
```

#### `equaling`

``` purescript
equaling :: forall a b. (Eq b) => (a -> b) -> a -> a -> Boolean
```

Given a function `a -> b`, create a new binary function which takes two
`a` values and returns true if and only if the results of the given
function are equal.

#### `Ordering`

``` purescript
data Ordering
  = LT
  | GT
  | EQ
```

##### Instances
``` purescript
instance semigroupOrdering :: Semigroup Ordering
instance eqOrdering :: Eq Ordering
instance ordOrdering :: Ord Ordering
instance boundedOrdering :: Bounded Ordering
instance showOrdering :: Show Ordering
```

The `Ordering` data type represents the three possible outcomes of
comparing two values:

`LT` - The first value is _less than_ the second.
`GT` - The first value is _greater than_ the second.
`EQ` - The first value is _equal to_ or _incomparable to_ the second.

#### `Ord`

``` purescript
class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering
```

##### Instances
``` purescript
instance ordBoolean :: Ord Boolean
instance ordInt :: Ord Int
instance ordNumber :: Ord Number
instance ordString :: Ord String
instance ordChar :: Ord Char
instance ordUnit :: Ord Unit
instance ordArray :: (Ord a) => Ord (Array a)
instance ordOrdering :: Ord Ordering
instance ordDesc :: (Ord a) => Ord (Desc a)
```

The `Ord` type class represents types which support comparisons.

`Ord` instances should satisfy the laws of _partially orderings_:

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`

#### `(<)`

``` purescript
(<) :: forall a. (Ord a) => a -> a -> Boolean
```

Test whether one value is _strictly less than_ another.

#### `(>)`

``` purescript
(>) :: forall a. (Ord a) => a -> a -> Boolean
```

Test whether one value is _strictly greater than_ another.

#### `(<=)`

``` purescript
(<=) :: forall a. (Ord a) => a -> a -> Boolean
```

Test whether one value is _non-strictly less than_ another.

#### `(>=)`

``` purescript
(>=) :: forall a. (Ord a) => a -> a -> Boolean
```

Test whether one value is _non-strictly greater than_ another.

#### `min`

``` purescript
min :: forall a. (Ord a) => a -> a -> a
```

Choose the smaller of two values. If they compare `EQ`, the first is
chosen. For example: `min 0 1 == 0`.

#### `max`

``` purescript
max :: forall a. (Ord a) => a -> a -> a
```

Choose the larger of two values. If they compare `EQ`, the first is
chosen. For example: `max 0 1 == 1`.

#### `clamp`

``` purescript
clamp :: forall a. (Ord a) => a -> a -> a -> a
```

Ensure that a value is between a lower and upper bound. For example:
`let f = clamp 0 10 in map f [-5, 5, 15] == [0, 5, 10]`

#### `between`

``` purescript
between :: forall a. (Ord a) => a -> a -> a -> Boolean
```

Test if a value is between two other values. For example:
`let f = between 0 10 in map f [-5, 5, 15] = [false, true, false]`

#### `ascending`

``` purescript
ascending :: forall a b. (Ord b) => (a -> b) -> a -> a -> Ordering
```

Compare two values based on the results of applying the given function
to both of them. Useful with functions like `Data.Array.sortBy`. For
example, `sortBy (ascending _.x)` will sort an array of records by their
`x` property, in ascending order.

#### `descending`

``` purescript
descending :: forall a b. (Ord b) => (a -> b) -> a -> a -> Ordering
```

Compare two values based on the results of applying the given function
to both of them, but flipped. Useful with functions like
`Data.Array.sortBy`. For example, `sortBy (descending _.x)` will sort an
array of records by their `x` property, in descending order.

#### `Desc`

``` purescript
newtype Desc a
  = Desc a
```

##### Instances
``` purescript
instance eqDesc :: (Eq a) => Eq (Desc a)
instance ordDesc :: (Ord a) => Ord (Desc a)
```

The `Desc` newtype reverses the order of a type's `Ord` instance. For
example: `Desc 5 < Desc 6 == true`.

#### `Bounded`

``` purescript
class (Ord a) <= Bounded a where
  top :: a
  bottom :: a
```

##### Instances
``` purescript
instance boundedBoolean :: Bounded Boolean
instance boundedUnit :: Bounded Unit
instance boundedOrdering :: Bounded Ordering
instance boundedInt :: Bounded Int
```

The `Bounded` type class represents types that are finite partially
ordered sets.

Instances should satisfy the following law in addition to the `Ord` laws:

- Ordering: `bottom <= a <= top`

#### `Lattice`

``` purescript
class (Ord a) <= Lattice a where
  sup :: a -> a -> a
  inf :: a -> a -> a
```

##### Instances
``` purescript
instance latticeBoolean :: Lattice Boolean
instance latticeUnit :: Lattice Unit
```

The `Lattice` type class represents types that are partially ordered
sets with a supremum (`sup` or `||`) and infimum (`inf` or `&&`).

Instances should satisfy the following laws in addition to the `Ord`
laws:

- Supremum:
  - `a || b >= a`
  - `a || b >= b`
- Infimum:
  - `a && b <= a`
  - `a && b <= b`
- Associativity:
  - `a || (b || c) = (a || b) || c`
  - `a && (b && c) = (a && b) && c`
- Commutativity:
  - `a || b = b || a`
  - `a && b = b && a`
- Absorption:
  - `a || (a && b) = a`
  - `a && (a || b) = a`
- Idempotent:
  - `a || a = a`
  - `a && a = a`

#### `(||)`

``` purescript
(||) :: forall a. (Lattice a) => a -> a -> a
```

The `sup` operator.

#### `(&&)`

``` purescript
(&&) :: forall a. (Lattice a) => a -> a -> a
```

The `inf` operator.

#### `BoundedLattice`

``` purescript
class (Bounded a, Lattice a) <= BoundedLattice a
```

##### Instances
``` purescript
instance boundedLatticeBoolean :: BoundedLattice Boolean
instance boundedLatticeUnit :: BoundedLattice Unit
```

The `BoundedLattice` type class represents types that are finite
lattices.

Instances should satisfy the following law in addition to the `Lattice`
and `Bounded` laws:

- Identity:
  - `a || bottom = a`
  - `a && top = a`
- Annihiliation:
  - `a || top = top`
  - `a && bottom = bottom`

#### `ComplementedLattice`

``` purescript
class (BoundedLattice a) <= ComplementedLattice a where
  not :: a -> a
```

##### Instances
``` purescript
instance complementedLatticeBoolean :: ComplementedLattice Boolean
instance complementedLatticeUnit :: ComplementedLattice Unit
```

The `ComplementedLattice` type class represents types that are lattices
where every member is also uniquely complemented.

Instances should satisfy the following law in addition to the
`BoundedLattice` laws:

- Complemented:
  - `not a || a == top`
  - `not a && a == bottom`
- Double negation:
  - `not <<< not == id`

#### `DistributiveLattice`

``` purescript
class (Lattice a) <= DistributiveLattice a
```

##### Instances
``` purescript
instance distributiveLatticeBoolean :: DistributiveLattice Boolean
instance distributiveLatticeUnit :: DistributiveLattice Unit
```

The `DistributiveLattice` type class represents types that are lattices
where the `&&` and `||` distribute over each other.

Instances should satisfy the following law in addition to the `Lattice`
laws:

- Distributivity: `x && (y || z) = (x && y) || (x && z)`

#### `BooleanAlgebra`

``` purescript
class (ComplementedLattice a, DistributiveLattice a) <= BooleanAlgebra a
```

##### Instances
``` purescript
instance booleanAlgebraBoolean :: BooleanAlgebra Boolean
instance booleanAlgebraUnit :: BooleanAlgebra Unit
```

The `BooleanAlgebra` type class represents types that are Boolean
algebras, also known as Boolean lattices.

Instances should satisfy the `ComplementedLattice` and
`DistributiveLattice` laws.

#### `Show`

``` purescript
class Show a where
  show :: a -> String
```

##### Instances
``` purescript
instance showBoolean :: Show Boolean
instance showInt :: Show Int
instance showNumber :: Show Number
instance showChar :: Show Char
instance showString :: Show String
instance showUnit :: Show Unit
instance showArray :: (Show a) => Show (Array a)
instance showOrdering :: Show Ordering
```

The `Show` type class represents those types which can be converted into
a human-readable `String` representation.

While not required, it is recommended that for any expression `x`, the
string `show x` be executable PureScript code which evaluates to the same
value as the expression `x`.


