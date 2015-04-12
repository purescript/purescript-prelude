# Module Documentation

## Module Prelude

#### `Unit`

``` purescript
newtype Unit
  = Unit {  }
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

#### `cons`

``` purescript
cons :: forall a. a -> [a] -> [a]
```

Attaches an element to the front of an array, creating a new array.

```purescript
cons 1 [2, 3, 4] = [1, 2, 3, 4]
```

Note, the running time of this function is `O(n)`.

#### `(:)`

``` purescript
(:) :: forall a. a -> [a] -> [a]
```

An infix alias for `cons`.

Note, the running time of this function is `O(n)`.

#### `Semigroupoid`

``` purescript
class Semigroupoid a where
  (<<<) :: forall b c d. a c d -> a b c -> a b d
```

A `Semigroupoid` is similar to a [`Category`](#category) but does not
require an identity element `id`, just composable morphisms.

`Semigroupoid`s must satisfy the following law:

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`

One example of a `Semigroupoid` is the function type constructor `(->)`,
with `(<<<)` defined as function composition.

#### `semigroupoidArr`

``` purescript
instance semigroupoidArr :: Semigroupoid Prim.Function
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

`Category`s consist of objects and composable morphisms between them, and
as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
must have an identity element.

Instances must satisfy the following law in addition to the
`Semigroupoid` law:

- Identity: `id <<< p = p <<< id = p`

#### `categoryArr`

``` purescript
instance categoryArr :: Category Prim.Function
```


#### `Functor`

``` purescript
class Functor f where
  (<$>) :: forall a b. (a -> b) -> f a -> f b
```

A `Functor` is a type constructor which supports a mapping operation
`(<$>)`.

`(<$>)` can be used to turn functions `a -> b` into functions
`f a -> f b` whose argument and return types use the type constructor `f`
to represent some computational context.

Instances must satisfy the following laws:

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`

#### `functorArr`

``` purescript
instance functorArr :: Functor (Prim.Function r)
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
  (<*>) :: forall a b. f (a -> b) -> f a -> f b
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

#### `applyArr`

``` purescript
instance applyArr :: Apply (Prim.Function r)
```


#### `Applicative`

``` purescript
class (Apply f) <= Applicative f where
  pure :: forall a. a -> f a
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

#### `applicativeArr`

``` purescript
instance applicativeArr :: Applicative (Prim.Function r)
```


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
  (<$>) = liftA1
```

#### `Bind`

``` purescript
class (Apply m) <= Bind m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
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

#### `bindArr`

``` purescript
instance bindArr :: Bind (Prim.Function r)
```


#### `Monad`

``` purescript
class (Applicative m, Bind m) <= Monad m where
```

The `Monad` type class combines the operations of the `Bind` and
`Applicative` type classes. Therefore, `Monad` instances represent type
constructors which support sequential composition, and also lifting of
functions of arbitrary arity.

Instances must satisfy the following laws in addition to the
`Applicative` and `Bind` laws:

- Left Identity: `pure x >>= f = f x`
- Right Identity: `x >>= pure = x`

#### `monadArr`

``` purescript
instance monadArr :: Monad (Prim.Function r)
```


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
  (<$>) = liftM1
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
  (<*>) = ap
```

#### `Semigroup`

``` purescript
class Semigroup a where
  (<>) :: a -> a -> a
```

The `Semigroup` type class identifies an associative operation on a type.

Instances are required to satisfy the following law:

- Associativity: `(x <> y) <> z = x <> (y <> z)`

One example of a `Semigroup` is `String`, with `(<>)` defined as string
concatenation.

#### `(++)`

``` purescript
(++) :: forall s. (Semigroup s) => s -> s -> s
```

`(++)` is an alias for `(<>)`.

#### `semigroupString`

``` purescript
instance semigroupString :: Semigroup String
```


#### `semigroupUnit`

``` purescript
instance semigroupUnit :: Semigroup Unit
```


#### `semigroupArr`

``` purescript
instance semigroupArr :: (Semigroup s') => Semigroup (s -> s')
```


#### `semigroupOrdering`

``` purescript
instance semigroupOrdering :: Semigroup Ordering
```


#### `Semiring`

``` purescript
class Semiring a where
  (+) :: a -> a -> a
  zero :: a
  (*) :: a -> a -> a
  one :: a
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

#### `semiringNumber`

``` purescript
instance semiringNumber :: Semiring Number
```


#### `semiringUnit`

``` purescript
instance semiringUnit :: Semiring Unit
```


#### `Ring`

``` purescript
class (Semiring a) <= Ring a where
  (-) :: a -> a -> a
```

The `Ring` class is for types that support addition, multiplication,
and subtraction operations.

Instances must satisfy the following law in addition to the `Semiring`
laws:

- Additive inverse: `a + (-a) = (-a) + a = zero`

#### `ringNumber`

``` purescript
instance ringNumber :: Ring Number
```


#### `ringUnit`

``` purescript
instance ringUnit :: Ring Unit
```


#### `negate`

``` purescript
negate :: forall a. (Ring a) => a -> a
```


#### `ModuloSemiring`

``` purescript
class (Semiring a) <= ModuloSemiring a where
  (/) :: a -> a -> a
  mod :: a -> a -> a
```

The `ModuloSemiring` class is for types that support addition,
multiplication, division, and modulo (division remainder) operations.

Instances must satisfy the following law in addition to the `Semiring`
laws:

- Remainder: `a / b * b + (a `mod` b) = a`

#### `moduloSemiringNumber`

``` purescript
instance moduloSemiringNumber :: ModuloSemiring Number
```


#### `moduloSemiringUnit`

``` purescript
instance moduloSemiringUnit :: ModuloSemiring Unit
```


#### `DivisionRing`

``` purescript
class (Ring a, ModuloSemiring a) <= DivisionRing a where
```

A `Ring` where every nonzero element has a multiplicative inverse.

Instances must satisfy the following law in addition to the `Ring` and
`ModuloSemiring` laws:

- Multiplicative inverse: `(one / x) * x = one`

As a consequence of this ```a `mod` b = zero``` as no divide operation
will have a remainder.

#### `divisionRingNumber`

``` purescript
instance divisionRingNumber :: DivisionRing Number
```


#### `divisionRingUnit`

``` purescript
instance divisionRingUnit :: DivisionRing Unit
```


#### `Num`

``` purescript
class (DivisionRing a) <= Num a where
```

The `Num` class is for types that are commutative fields.

Instances must satisfy the following law in addition to the
`DivisionRing` laws:

- Commutative multiplication: `a * b = b * a`

#### `numNumber`

``` purescript
instance numNumber :: Num Number
```


#### `numUnit`

``` purescript
instance numUnit :: Num Unit
```


#### `Eq`

``` purescript
class Eq a where
  (==) :: a -> a -> Boolean
  (/=) :: a -> a -> Boolean
```

The `Eq` type class represents types which support decidable equality.

`Eq` instances should satisfy the following laws:

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`
- Negation: `x /= y = not (x == y)`

`(/=)` may be implemented in terms of `(==)`, but it might give a performance improvement to implement it separately.

#### `eqBoolean`

``` purescript
instance eqBoolean :: Eq Boolean
```


#### `eqNumber`

``` purescript
instance eqNumber :: Eq Number
```


#### `eqString`

``` purescript
instance eqString :: Eq String
```


#### `eqUnit`

``` purescript
instance eqUnit :: Eq Unit
```


#### `eqArray`

``` purescript
instance eqArray :: (Eq a) => Eq [a]
```


#### `eqOrdering`

``` purescript
instance eqOrdering :: Eq Ordering
```


#### `Ordering`

``` purescript
data Ordering
  = LT 
  | GT 
  | EQ 
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

The `Ord` type class represents types which support comparisons.

`Ord` instances should satisfy the laws of _partially orderings_:

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`

#### `ordBoolean`

``` purescript
instance ordBoolean :: Ord Boolean
```


#### `ordNumber`

``` purescript
instance ordNumber :: Ord Number
```


#### `ordString`

``` purescript
instance ordString :: Ord String
```


#### `ordUnit`

``` purescript
instance ordUnit :: Ord Unit
```


#### `ordArray`

``` purescript
instance ordArray :: (Ord a) => Ord [a]
```


#### `ordOrdering`

``` purescript
instance ordOrdering :: Ord Ordering
```


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

#### `Bounded`

``` purescript
class (Ord a) <= Bounded a where
  top :: a
  bottom :: a
```

The `Bounded` type class represents types that are finite partially
ordered sets.

Instances should satisfy the following law in addition to the `Ord` laws:

- Ordering: `bottom <= a <= top`

#### `boundedBoolean`

``` purescript
instance boundedBoolean :: Bounded Boolean
```


#### `boundedUnit`

``` purescript
instance boundedUnit :: Bounded Unit
```


#### `boundedOrdering`

``` purescript
instance boundedOrdering :: Bounded Ordering
```


#### `Lattice`

``` purescript
class (Ord a) <= Lattice a where
  sup :: a -> a -> a
  inf :: a -> a -> a
```

The `Lattice` type class represents types that are partially ordered
sets with a supremum (`sup` or `||`) and infimum (`inf` or `&&`).

Instances should satisfy the following laws in addition to the `Ord`
laws:

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

#### `latticeBoolean`

``` purescript
instance latticeBoolean :: Lattice Boolean
```


#### `latticeUnit`

``` purescript
instance latticeUnit :: Lattice Unit
```


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
class (Bounded a, Lattice a) <= BoundedLattice a where
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

#### `boundedLatticeBoolean`

``` purescript
instance boundedLatticeBoolean :: BoundedLattice Boolean
```


#### `boundedLatticeUnit`

``` purescript
instance boundedLatticeUnit :: BoundedLattice Unit
```


#### `ComplementedLattice`

``` purescript
class (BoundedLattice a) <= ComplementedLattice a where
  not :: a -> a
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

#### `complementedLatticeBoolean`

``` purescript
instance complementedLatticeBoolean :: ComplementedLattice Boolean
```


#### `complementedLatticeUnit`

``` purescript
instance complementedLatticeUnit :: ComplementedLattice Unit
```


#### `DistributiveLattice`

``` purescript
class (Lattice a) <= DistributiveLattice a where
```

The `DistributiveLattice` type class represents types that are lattices
where the `&&` and `||` distribute over each other.

Instances should satisfy the following law in addition to the `Lattice`
laws:

- Distributivity: `x && (y || z) = (x && y) || (x && z)`

#### `distributiveLatticeBoolean`

``` purescript
instance distributiveLatticeBoolean :: DistributiveLattice Boolean
```


#### `distributiveLatticeUnit`

``` purescript
instance distributiveLatticeUnit :: DistributiveLattice Unit
```


#### `BooleanAlgebra`

``` purescript
class (ComplementedLattice a, DistributiveLattice a) <= BooleanAlgebra a where
```

The `BooleanAlgebra` type class represents types that are Boolean
algebras, also known as Boolean lattices.

Instances should satisfy the `ComplementedLattice` and
`DistributiveLattice` laws.

#### `booleanAlgebraBoolean`

``` purescript
instance booleanAlgebraBoolean :: BooleanAlgebra Boolean
```


#### `booleanAlgebraUnit`

``` purescript
instance booleanAlgebraUnit :: BooleanAlgebra Unit
```


#### `Show`

``` purescript
class Show a where
  show :: a -> String
```

The `Show` type class represents those types which can be converted into
a human-readable `String` representation.

While not required, it is recommended that for any expression `x`, the
string `show x` be executable PureScript code which evaluates to the same
value as the expression `x`.

#### `showBoolean`

``` purescript
instance showBoolean :: Show Boolean
```


#### `showNumber`

``` purescript
instance showNumber :: Show Number
```


#### `showString`

``` purescript
instance showString :: Show String
```


#### `showUnit`

``` purescript
instance showUnit :: Show Unit
```


#### `showArray`

``` purescript
instance showArray :: (Show a) => Show [a]
```


#### `showOrdering`

``` purescript
instance showOrdering :: Show Ordering
```




