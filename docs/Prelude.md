## Module Prelude

#### `Unit`

``` purescript
newtype Unit
```

The `Unit` type has a single inhabitant, called `unit`. It represents
values with no computational content.

`Unit` is often used, wrapped in a monadic type constructor, as the
return type of a computation where only
the _effects_ are important.

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
instance boundedOrdUnit :: BoundedOrd Unit
instance booleanAlgebraUnit :: BooleanAlgebra Unit
instance showUnit :: Show Unit
```

#### `unit`

``` purescript
unit :: Unit
```

`unit` is the sole inhabitant of the `Unit` type.

#### `($)`

``` purescript
($) :: forall a b. (a -> b) -> a -> b
```

_right-associative / precedence 0_

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

_left-associative / precedence 1_

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

A `Semigroupoid` is similar to a [`Category`](#category) but does not
require an identity element `id`, just composable morphisms.

`Semigroupoid`s must satisfy the following law:

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`

One example of a `Semigroupoid` is the function type constructor `(->)`,
with `(<<<)` defined as function composition.

##### Instances
``` purescript
instance semigroupoidFn :: Semigroupoid Function
```

#### `(<<<)`

``` purescript
(<<<) :: forall a b c d. (Semigroupoid a) => a c d -> a b c -> a b d
```

_right-associative / precedence 9_

`(<<<)` is an alias for `compose`.

#### `(>>>)`

``` purescript
(>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
```

_right-associative / precedence 9_

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

##### Instances
``` purescript
instance categoryFn :: Category Function
```

#### `Functor`

``` purescript
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

A `Functor` is a type constructor which supports a mapping operation
`(<$>)`.

`(<$>)` can be used to turn functions `a -> b` into functions
`f a -> f b` whose argument and return types use the type constructor `f`
to represent some computational context.

Instances must satisfy the following laws:

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`

##### Instances
``` purescript
instance functorFn :: Functor (Function r)
instance functorArray :: Functor Array
```

#### `(<$>)`

``` purescript
(<$>) :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
```

_left-associative / precedence 4_

`(<$>)` is an alias for `map`

#### `(<#>)`

``` purescript
(<#>) :: forall f a b. (Functor f) => f a -> (a -> b) -> f b
```

_left-associative / precedence 1_

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

##### Instances
``` purescript
instance applyFn :: Apply (Function r)
instance applyArray :: Apply Array
```

#### `(<*>)`

``` purescript
(<*>) :: forall f a b. (Apply f) => f (a -> b) -> f a -> f b
```

_left-associative / precedence 4_

`(<*>)` is an alias for `apply`.

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

##### Instances
``` purescript
instance applicativeFn :: Applicative (Function r)
instance applicativeArray :: Applicative Array
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
  map = liftA1
```

#### `Bind`

``` purescript
class (Apply m) <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b
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

##### Instances
``` purescript
instance bindFn :: Bind (Function r)
instance bindArray :: Bind Array
```

#### `(>>=)`

``` purescript
(>>=) :: forall m a b. (Bind m) => m a -> (a -> m b) -> m b
```

_left-associative / precedence 1_

`(>>=)` is an alias for `bind`.

#### `Monad`

``` purescript
class (Applicative m, Bind m) <= Monad m
```

The `Monad` type class combines the operations of the `Bind` and
`Applicative` type classes. Therefore, `Monad` instances represent type
constructors which support sequential composition, and also lifting of
functions of arbitrary arity.

Instances must satisfy the following laws in addition to the
`Applicative` and `Bind` laws:

- Left Identity: `pure x >>= f = f x`
- Right Identity: `x >>= pure = x`

##### Instances
``` purescript
instance monadFn :: Monad (Function r)
instance monadArray :: Monad Array
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

The `Semigroup` type class identifies an associative operation on a type.

Instances are required to satisfy the following law:

- Associativity: `(x <> y) <> z = x <> (y <> z)`

One example of a `Semigroup` is `String`, with `(<>)` defined as string
concatenation.

##### Instances
``` purescript
instance semigroupString :: Semigroup String
instance semigroupUnit :: Semigroup Unit
instance semigroupFn :: (Semigroup s') => Semigroup (s -> s')
instance semigroupOrdering :: Semigroup Ordering
instance semigroupArray :: Semigroup (Array a)
```

#### `(<>)`

``` purescript
(<>) :: forall s. (Semigroup s) => s -> s -> s
```

_right-associative / precedence 5_

`(<>)` is an alias for `append`.

#### `(++)`

``` purescript
(++) :: forall s. (Semigroup s) => s -> s -> s
```

_right-associative / precedence 5_

`(++)` is an alternative alias for `append`.

#### `Semiring`

``` purescript
class Semiring a where
  add :: a -> a -> a
  zero :: a
  mul :: a -> a -> a
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

##### Instances
``` purescript
instance semiringInt :: Semiring Int
instance semiringNumber :: Semiring Number
instance semiringUnit :: Semiring Unit
```

#### `(+)`

``` purescript
(+) :: forall a. (Semiring a) => a -> a -> a
```

_left-associative / precedence 6_

`(+)` is an alias for `add`.

#### `(*)`

``` purescript
(*) :: forall a. (Semiring a) => a -> a -> a
```

_left-associative / precedence 7_

`(*)` is an alias for `mul`.

#### `Ring`

``` purescript
class (Semiring a) <= Ring a where
  sub :: a -> a -> a
```

The `Ring` class is for types that support addition, multiplication,
and subtraction operations.

Instances must satisfy the following law in addition to the `Semiring`
laws:

- Additive inverse: `a - a = (zero - a) + a = zero`

##### Instances
``` purescript
instance ringInt :: Ring Int
instance ringNumber :: Ring Number
instance ringUnit :: Ring Unit
```

#### `(-)`

``` purescript
(-) :: forall a. (Ring a) => a -> a -> a
```

_left-associative / precedence 6_

`(-)` is an alias for `sub`.

#### `negate`

``` purescript
negate :: forall a. (Ring a) => a -> a
```

`negate x` can be used as a shorthand for `zero - x`.

#### `ModuloSemiring`

``` purescript
class (Semiring a) <= ModuloSemiring a where
  div :: a -> a -> a
  mod :: a -> a -> a
```

The `ModuloSemiring` class is for types that support addition,
multiplication, division, and modulo (division remainder) operations.

Instances must satisfy the following law in addition to the `Semiring`
laws:

- Remainder: ``a / b * b + (a `mod` b) = a``

##### Instances
``` purescript
instance moduloSemiringInt :: ModuloSemiring Int
instance moduloSemiringNumber :: ModuloSemiring Number
instance moduloSemiringUnit :: ModuloSemiring Unit
```

#### `(/)`

``` purescript
(/) :: forall a. (ModuloSemiring a) => a -> a -> a
```

_left-associative / precedence 7_

`(/)` is an alias for `div`.

#### `DivisionRing`

``` purescript
class (Ring a, ModuloSemiring a) <= DivisionRing a
```

A `Ring` where every nonzero element has a multiplicative inverse.

Instances must satisfy the following law in addition to the `Ring` and
`ModuloSemiring` laws:

- Multiplicative inverse: `(one / x) * x = one`

As a consequence of this ```a `mod` b = zero``` as no divide operation
will have a remainder.

##### Instances
``` purescript
instance divisionRingNumber :: DivisionRing Number
instance divisionRingUnit :: DivisionRing Unit
```

#### `Num`

``` purescript
class (DivisionRing a) <= Num a
```

The `Num` class is for types that are commutative fields.

Instances must satisfy the following law in addition to the
`DivisionRing` laws:

- Commutative multiplication: `a * b = b * a`

##### Instances
``` purescript
instance numNumber :: Num Number
instance numUnit :: Num Unit
```

#### `Eq`

``` purescript
class Eq a where
  eq :: a -> a -> Boolean
```

The `Eq` type class represents types which support decidable equality.

`Eq` instances should satisfy the following laws:

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`

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
```

#### `(==)`

``` purescript
(==) :: forall a. (Eq a) => a -> a -> Boolean
```

_non-associative / precedence 4_

`(==)` is an alias for `eq`. Tests whether one value is equal to another.

#### `(/=)`

``` purescript
(/=) :: forall a. (Eq a) => a -> a -> Boolean
```

_non-associative / precedence 4_

`(/=)` tests whether one value is _not equal_ to another. Shorthand for
`not (x == y)`.

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
`EQ` - The first value is _equal to_ the second.

##### Instances
``` purescript
instance semigroupOrdering :: Semigroup Ordering
instance eqOrdering :: Eq Ordering
instance ordOrdering :: Ord Ordering
instance boundedOrdering :: Bounded Ordering
instance boundedOrdOrdering :: BoundedOrd Ordering
instance showOrdering :: Show Ordering
```

#### `Ord`

``` purescript
class (Eq a) <= Ord a where
  compare :: a -> a -> Ordering
```

The `Ord` type class represents types which support comparisons with a
_total order_.

`Ord` instances should satisfy the laws of total orderings:

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`

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
```

#### `(<)`

``` purescript
(<) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _strictly less than_ another.

#### `(>)`

``` purescript
(>) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _strictly greater than_ another.

#### `(<=)`

``` purescript
(<=) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _non-strictly less than_ another.

#### `(>=)`

``` purescript
(>=) :: forall a. (Ord a) => a -> a -> Boolean
```

_left-associative / precedence 4_

Test whether one value is _non-strictly greater than_ another.

#### `unsafeCompare`

``` purescript
unsafeCompare :: forall a. a -> a -> Ordering
```

#### `Bounded`

``` purescript
class Bounded a where
  top :: a
  bottom :: a
```

The `Bounded` type class represents types that are finite.

Although there are no "internal" laws for `Bounded`, every value of `a`
should be considered less than or equal to `top` by some means, and greater
than or equal to `bottom`.

The lack of explicit `Ord` constraint allows flexibility in the use of
`Bounded` so it can apply to total and partially ordered sets, boolean
algebras, etc.

##### Instances
``` purescript
instance boundedBoolean :: Bounded Boolean
instance boundedUnit :: Bounded Unit
instance boundedOrdering :: Bounded Ordering
instance boundedInt :: Bounded Int
instance boundedChar :: Bounded Char
instance boundedFn :: (Bounded b) => Bounded (a -> b)
```

#### `BoundedOrd`

``` purescript
class (Bounded a, Ord a) <= BoundedOrd a
```

The `BoundedOrd` type class represents totally ordered finite data types.

Instances should satisfy the following law in addition to the `Ord` laws:

- Ordering: `bottom <= a <= top`

##### Instances
``` purescript
instance boundedOrdBoolean :: BoundedOrd Boolean
instance boundedOrdUnit :: BoundedOrd Unit
instance boundedOrdOrdering :: BoundedOrd Ordering
instance boundedOrdInt :: BoundedOrd Int
instance boundedOrdChar :: BoundedOrd Char
```

#### `BooleanAlgebra`

``` purescript
class (Bounded a) <= BooleanAlgebra a where
  conj :: a -> a -> a
  disj :: a -> a -> a
  not :: a -> a
```

The `BooleanAlgebra` type class represents types that behave like boolean
values.

Instances should satisfy the following laws in addition to the `Bounded`
laws:

- Associativity:
  - `a || (b || c) = (a || b) || c`
  - `a && (b && c) = (a && b) && c`
- Commutativity:
  - `a || b = b || a`
  - `a && b = b && a`
- Distributivity:
  - `a && (b || c) = (a && b) || (a && c)`
  - `a || (b && c) = (a || b) && (a || c)`
- Identity:
  - `a || bottom = a`
  - `a && top = a`
- Idempotent:
  - `a || a = a`
  - `a && a = a`
- Absorption:
  - `a || (a && b) = a`
  - `a && (a || b) = a`
- Annhiliation:
  - `a || top = top`
- Complementation:
  - `a && not a = bottom`
  - `a || not a = top`

##### Instances
``` purescript
instance booleanAlgebraBoolean :: BooleanAlgebra Boolean
instance booleanAlgebraUnit :: BooleanAlgebra Unit
instance booleanAlgebraFn :: (BooleanAlgebra b) => BooleanAlgebra (a -> b)
```

#### `(&&)`

``` purescript
(&&) :: forall a. (BooleanAlgebra a) => a -> a -> a
```

_right-associative / precedence 3_

`(&&)` is an alias for `conj`.

#### `(||)`

``` purescript
(||) :: forall a. (BooleanAlgebra a) => a -> a -> a
```

_right-associative / precedence 2_

`(||)` is an alias for `disj`.

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


