-- | This module provides the `Repr` type, which may be used as a common
-- | format for representations of values. The `Repr` type is intended for
-- | use in testing, debugging, and in the REPL, but not in production code.
module Data.Debugged.Type
  -- Data type and construction
  ( Repr
  , int
  , number
  , boolean
  , char
  , string
  , array
  , record
  , constructor
  , opaque
  , collection
  , assoc

  -- pretty printing
  , prettyPrint

  -- diffing
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, foldMap, fold)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

-------------------------------------------------------------------------------
-- BASIC DATA TYPES -----------------------------------------------------------

-- | A strict rose tree type. Based on Data.Tree in Haskell's `containers`.
data Tree a
  = Node a (Array (Tree a))

rootLabel :: forall a. Tree a -> a
rootLabel (Node r _) = r

children :: forall a. Tree a -> Array (Tree a)
children (Node _ xs) = xs

isLeaf :: forall a. Tree a -> Boolean
isLeaf (Node _ []) = true
isLeaf _ = false

derive instance functorTree :: Functor Tree

-- | Fold a tree bottom-up; the function `foldTree f` applies `f []` to each of
-- | the leaf nodes, and then works its way up the internal nodes, applying `f`
-- | to:
-- | - the label at the current node, and
-- | - the result of applying `f` to each child,
-- | finishing at the root.
foldTree :: forall a b. (a -> Array b -> b) -> Tree a -> b
foldTree f = go
  where
  go (Node x ts) = f x (map go ts)

-------------------------------------------------------------------------------
-- THE REPR TYPE & CONSTRUCTORS -----------------------------------------------

-- | A debugging representation of some PureScript value. It is often possible
-- | to reconstruct the original value from this representation, but not
-- | always. Notable counterexamples are `Effect`, `Ref`, and `(->)`.
newtype Repr = Repr (Tree Label)

unRepr :: Repr -> Tree Label
unRepr (Repr tree) = tree

-- | Labels for a tree which encodes a debugging representation of some
-- | PureScript value.
data Label
  -- Note that Repr trees are not correct-by-construction by default; whether
  -- or not children are permitted at a given node depends on the constructor
  -- of the label. We do however use newtypes, and we avoid exporting
  -- constructors in favour of specialised construction functions, in order to
  -- ensure that the exposed API may only be used to construct correct trees.

  -- These labels should always be leaves, and should only ever be used to
  -- represent the Prim types they contain.
  = Int Int
  | Number Number
  | Boolean Boolean
  | Char Char
  | String String

  -- This label represents a value of the type `Prim.Array`. Its children
  -- should be the contents of the array.
  | Array

  -- This label represents a value of the type `Prim.Record`. Its immediate
  -- children should all have Prop labels.
  | Record

  -- This node represents a property with the given name. It should only occur
  -- as a direct descendent of a Record- or Opaque-labelled node.  A node
  -- with this label should always have exactly one child: the property value.
  | Prop String

  -- Function application; mostly intended for use with data constructors.
  -- Children of nodes with this label represent the arguments to the
  -- function/data constructor.
  | App String

  -- These constructors are for representations of opaque data types, or data
  -- types where this representation is more helpful than the 'obvious'
  -- representation (e.g. List).

  -- This constructor represents an opaque data type such as `(->)` or `Ref`.
  -- The argument should contain the name of the data type. Nodes with this
  -- label should only have Prop labels as immediate children.
  | Opaque String

  -- This constructor represents a list-like collection. The argument should
  -- contain the name of the data type.
  | Collection String

  -- This constructor is for map-like collections. The children are the
  -- contents; every direct child should have an AssocProp label.
  | Assoc String

  -- This constructor is for the key-value pairs of map-like collections. Each
  -- node with this label should have exactly two children: the key and the
  -- value.
  | AssocProp

derive instance eqLabel :: Eq Label
derive instance ordLabel :: Ord Label

-- | If a given tree is a correctly-formed Prop, apply the given function to
-- | its name and its value and return the result. Note that a correctly-formed
-- | Prop always has precisely one child: its value.
withProp :: forall a. (String -> Tree Label -> a) -> Tree Label -> Maybe a
withProp f =
  case _ of
    Node (Prop name) [value] ->
      Just (f name value)
    _ ->
      Nothing

-- | If a given tree is a correctly-formed AssocProp, apply the given function
-- | to its key and its value and return the result. Note that a
-- | correctly-formed AssocProp always has precisely two children: its key and
-- | its value.
withAssocProp :: forall a. (Tree Label -> Tree Label -> a) -> Tree Label -> Maybe a
withAssocProp f =
  case _ of
    Node AssocProp [key, value] ->
      Just (f key value)
    _ ->
      Nothing

leaf :: Label -> Repr
leaf label = Repr (Node label [])

int :: Int -> Repr
int = leaf <<< Int

number :: Number -> Repr
number = leaf <<< Number

boolean :: Boolean -> Repr
boolean = leaf <<< Boolean

char :: Char -> Repr
char = leaf <<< Char

string :: String -> Repr
string = leaf <<< String

array :: Array Repr -> Repr
array = Repr <<< Node Array <<< map unRepr

record :: Array (Tuple String Repr) -> Repr
record =
  Repr <<< Node Record <<< makeProps

makeProps :: Array (Tuple String Repr) -> Array (Tree Label)
makeProps = map unwrapProp
  where
  unwrapProp :: Tuple String Repr -> Tree Label
  unwrapProp (Tuple name (Repr val)) =
    Node (Prop name) [val]

constructor :: String -> Array Repr -> Repr
constructor name args =
  Repr (Node (App name) (map unRepr args))

opaque :: String -> Array (Tuple String Repr) -> Repr
opaque name props =
  Repr (Node (Opaque name) (makeProps props))

collection :: String -> Array Repr -> Repr
collection name contents =
  Repr (Node (Collection name) (map unRepr contents))

assoc :: String -> Array (Tuple Repr Repr) -> Repr
assoc name contents =
  Repr (Node (Assoc name) (map makeAssocProp contents))
  where
  makeAssocProp :: Tuple Repr Repr -> Tree Label
  makeAssocProp (Tuple (Repr k) (Repr v)) = Node AssocProp [k, v]

-------------------------------------------------------------------------------
-- PRETTY-PRINTING ------------------------------------------------------------

prettyPrint :: Repr -> String
prettyPrint = String.joinWith "\n" <<< go <<< unRepr
  where
  go :: Tree Label -> Array String
  go tree =
    if isSmall tree
      then compact (goExpanded tree)
      else goExpanded tree

  goExpanded :: Tree Label -> Array String
  goExpanded tree =
    case rootLabel tree of
      Int x ->
        line (show x)
      Number x ->
        line (show x)
      Boolean x ->
        line (show x)
      Char x ->
        line (show x)
      String x ->
        line (show x)
      App name ->
        line name <> foldMap (map ("  " <> _) <<< goAtom) (children tree)
      Array ->
        commaSeq "[ " " ]" (map go (children tree))
      Record ->
        commaSeq "{ " " }" (Array.mapMaybe goRecordProp (children tree))
      Opaque name ->
        surround "<" ">" $
          line name
          <> map ("  " <> _)
              (Array.filter (_ /= "")
                (commaSeq "" "" (Array.mapMaybe goOpaqueProp (children tree))))
      Collection name ->
        surround "<" ">" $
          line name
          <> commaSeq "[ " " ]" (map go (children tree))
      Assoc name ->
        surround "<" ">" $
          line name
          <> commaSeq "{ " " }" (Array.mapMaybe goAssocProp (children tree))

      -- should not happen
      AssocProp ->
        []
      Prop _ ->
        []

  -- Reduce a multiple-line pretty-printed expression down to one line.
  compact :: Array String -> Array String
  compact =
    firstMiddleLast >>>
    case _ of
      Empty ->
        []
      Single x ->
        [x]
      TwoOrMore first middle last ->
        [String.joinWith " "
          ([first] <> map String.trim middle <> [String.trim last])]

  goAtom tree =
    if needsParens tree
      then surround "(" ")" (go tree)
      else go tree

  goRecordProp =
    withProp \name val ->
      line (name <> ":") <> map ("    " <> _) (go val)

  goOpaqueProp =
    withProp \name val ->
      line (name <> ":") <> map ("  " <> _) (go val)

  goAssocProp =
    withAssocProp \key val ->
      withLast (_ <> ":") (go key)
      <> go val

  line c = [c]

  withLast f xs =
    case Array.length xs of
      0 ->
        []
      n ->
        unsafePartial $
          Array.slice 0 (n-2) xs <> [f (Array.unsafeIndex xs (n-1))]

data FirstMiddleLast a
  = Empty
  | Single a
  | TwoOrMore a (Array a) a

firstMiddleLast :: forall a. Array a -> FirstMiddleLast a
firstMiddleLast =
  case _ of
    [] ->
      Empty
    [x] ->
      Single x
    xs ->
      let
        n = Array.length xs
      in
        unsafePartial $
          TwoOrMore
            (Array.unsafeIndex xs 0)
            (Array.slice 1 (n-1) xs)
            (Array.unsafeIndex xs (n-1))

-- | Produce a comma separated sequence over multiple lines with the given
-- | beginning and ending string sequences.
commaSeq :: String -> String -> Array (Array String) -> Array String
commaSeq begin end =
  firstMiddleLast >>>
  case _ of
    Empty ->
      [ begin <> end ]
    Single item ->
      surround begin end item
    TwoOrMore first middle last ->
      surround begin "," first
      <> Array.concatMap (surround spacer ",") middle
      <> surround spacer end last

  where
  spacer = fold (Array.replicate (String.length begin) " ")

surround :: String -> String -> Array String -> Array String
surround start finish =
  firstMiddleLast >>>
  case _ of
    Empty ->
      [ start <> finish ]
    Single item ->
      [ start <> item <> finish ]
    TwoOrMore first middle last ->
      [ start <> first ]
      <> middle
      <> [ last <> finish ]

-- | A somewhat arbitrary heuristic to decide whether a subtree is sufficiently
-- | small to be allowed to be printed on one line.
isSmall :: Tree Label -> Boolean
isSmall subtree =
  let
    xs = children subtree
  in
    all isReallySmall xs && Array.length xs <= 5

-- | Is a subtree *really* small? Note that being a leaf neither sufficient nor
-- | necessary for a subtree to count as "really small" (although most leaves
-- | will be "really small").
isReallySmall :: Tree Label -> Boolean
isReallySmall subtree =
  case rootLabel subtree of
    Int _ ->
      true
    Number _ ->
      true
    Boolean _ ->
      true
    Char _ ->
      true
    String x ->
      String.length x <= 15
    Array ->
      ifOne (children subtree) isLeaf
    Record ->
      false
    Prop name ->
      internal name
    App name ->
      internal name
    Opaque name ->
      internal name
    Collection name ->
      internal name
    Assoc name ->
      false
    AssocProp ->
      all isLeaf (children subtree)

  where
  internal name =
    String.length name <= 10 && ifOne (children subtree) isLeaf

  ifOne [] _ = true
  ifOne [x] pred = pred x
  ifOne _ _ = false

-- | Check whether a subtree needs to be wrapped in parens when being
-- | displayed in a context which would require them (e.g. as an argument to
-- | a data constructor).
needsParens :: Tree Label -> Boolean
needsParens tree =
  case rootLabel tree of
    Int x ->
      x < 0
    Number x ->
      -- this slightly odd construction is to ensure we return true for any
      -- positive value and false for any negative value (including negative
      -- zero).
      (1.0 / x) < 0.0
    App _ ->
      not (isLeaf tree)
    _ ->
      false

-------------------------------------------------------------------------------
-- DIFFING --------------------------------------------------------------------

-- | A type for labels for delta trees. If a tree has labels of type `a`, then
-- | we can represent a delta tree with labels of type `Delta a`.
data Delta a
  -- Each occurrence of this label corresponds to a node where the two trees
  -- being compared are identical.
  = Same a

  -- This label indicates that the two trees being compared differ at the node
  -- being labelled. Every node with this label should have exactly two
  -- children: the first being the subtree rooted here in the first of the two
  -- trees being diffed, and the second being the subtree rooted here in the
  -- second tree.
  | Different

  -- This label indicates that the first of the trees being diffed has a
  -- subtree here, whereas the second does not. It should have exactly one
  -- child: the subtree of the first tree rooted at the point where it appears.
  | Extra1

  -- This label indicates that the second of the trees being diffed has a
  -- subtree here, whereas the second does not. It should have exactly one
  -- child: the subtree of the second tree rooted at the point where it
  -- appears.
  | Extra2

  -- This label indicates that we are in a differing subtree (and hence are not
  -- going to bother to perform any more diffing).
  | Subtree a

derive instance eqDelta :: Eq a => Eq (Delta a)
derive instance ordDelta :: Ord a => Ord (Delta a)

diff :: forall a. Eq a => Tree a -> Tree a -> Tree (Delta a)
diff = go
  where
  go left@(Node x xs) right@(Node y ys) =
    if x == y
      then Node (Same x) (goChildren xs ys)
      else Node Different [map Subtree left, map Subtree right]

  goChildren :: Array (Tree a) -> Array (Tree a) -> Array (Tree (Delta a))
  goChildren xs ys =
    let
      xlen = Array.length xs
      ylen = Array.length ys
      begin = Array.zipWith go xs ys
    in
      case compare xlen ylen of
        LT ->
          begin <> map (extra Extra1) (Array.drop xlen ys)
        EQ ->
          begin
        GT ->
          begin <> map (extra Extra2) (Array.drop ylen xs)

  extra :: Delta a -> Tree a -> Tree (Delta a)
  extra ctor subtree = Node ctor [map Subtree subtree]
