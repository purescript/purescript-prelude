-- | This module provides the basic building blocks which the `Debug` type
-- | class makes use of. It provides the `Repr` type and functions for
-- | constructing values of this type, the `ReprDelta` type for representing
-- | differences between two `Repr` values, and pretty-printing functions.
module Data.Debug.Type
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

  -- diffing
  , ReprDelta
  , diff

  -- pretty printing
  , prettyPrint
  , prettyPrintDelta
  ) where

import Prelude

import Data.Array as Array
import Data.Debug.PrettyPrinter (Content, commaSeq, compact, emptyContent, indent, leaf, noParens, noWrap, parens, printContent, surround, verbatim, wrap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..))

-------------------------------------------------------------------------------
-- BASIC DATA TYPES -----------------------------------------------------------

-- | A strict rose tree type. Based on Data.Tree in Haskell's `containers`.
data Tree a
  = Node a (Array (Tree a))

rootLabel :: forall a. Tree a -> a
rootLabel (Node r _) = r

subtrees :: forall a. Tree a -> Array (Tree a)
subtrees (Node _ xs) = xs

isLeaf :: forall a. Tree a -> Boolean
isLeaf (Node _ []) = true
isLeaf _ = false

derive instance eqTree :: Eq a => Eq (Tree a)
derive instance ordTree :: Ord a => Ord (Tree a)
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

derive newtype instance eqRepr :: Eq Repr
derive newtype instance ordRepr :: Ord Repr

unRepr :: Repr -> Tree Label
unRepr (Repr tree) = tree

-- | Labels for a tree which encodes a debugging representation of some
-- | PureScript value.
data Label
  -- Note that Repr trees are not correct-by-construction by default; whether
  -- or not children are permitted at a given node depends on the constructor
  -- of the label. We do however use newtypes, and we avoid exporting
  -- constructors in favour of specialised construction functions, in order to
  -- ensure that all trees constructed by the exposed API are guaranteed to
  -- be valid.

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

mkLeaf :: Label -> Repr
mkLeaf label = Repr (Node label [])

int :: Int -> Repr
int = mkLeaf <<< Int

number :: Number -> Repr
number = mkLeaf <<< Number

boolean :: Boolean -> Repr
boolean = mkLeaf <<< Boolean

char :: Char -> Repr
char = mkLeaf <<< Char

string :: String -> Repr
string = mkLeaf <<< String

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

-- | Pretty-print a `Repr` value; intended for use in e.g. the repl.
-- |
-- | The output will be executable PureScript code provided that the given
-- | `Repr` value does not contain any nodes which were constructed with the
-- | `opaque`, `collection`, or `assoc` functions.
prettyPrint :: Repr -> String
prettyPrint =
  printContent
  <<< foldTree (withResizing prettyPrintGo)
  <<< unRepr

prettyPrintDelta :: ReprDelta -> String
prettyPrintDelta =
  printContent
  <<< foldTree (withResizing prettyPrintGoDelta)
  <<< unReprDelta

prettyPrintSizeThreshold :: Int
prettyPrintSizeThreshold = 8

measure :: forall a. Sized a => a -> Array Content -> Int
measure root children =
  size root + unwrap (foldMap _.size children)

withResizing :: forall a. Sized a =>
  (a -> Array Content -> Content) ->
  (a -> Array Content -> Content)
withResizing f root children =
  if measure root children <= prettyPrintSizeThreshold
    then compact (f root children)
    else f root children

prettyPrintGo :: Label -> Array Content -> Content
prettyPrintGo root children =
  case root of
    Int x ->
      (leaf x) { needsParens = x < 0 }
    Number x ->
      -- this slightly odd construction is to ensure we return true for any
      -- positive value and false for any negative value (including
      -- negative zero).
      (leaf x) { needsParens = (1.0 / x) < 0.0 }
    Boolean x ->
      leaf x
    Char x ->
      leaf x
    String x ->
      leaf x
    App name ->
      parens (verbatim name <> foldMap (indent "  " <<< wrap) children)
    Array ->
      commaSeq "[ " " ]" children
    Record ->
      commaSeq "{ " " }" children
    Opaque name ->
      noParens $
        surround "<" ">" $
          verbatim name <> indent "  " (noWrap (commaSeq "" "" children))
    Collection name ->
      noParens $
        surround "<" ">" $
          verbatim name <> indent "  " (noWrap (commaSeq "[ " " ]" children))
    Assoc name ->
      noParens $
        surround "<" ">" $
          verbatim name <> noWrap (commaSeq "{ " " }" children)
    AssocProp ->
      case children of
        [key, val] ->
          noParens $ (surround "" ":" (noWrap key) <> noWrap val)
        _ ->
          -- should not happen
          emptyContent
    Prop name ->
      case children of
        [val] ->
          noParens $ verbatim (name <> ":") <> indent "  " (noWrap val)
        _ ->
          -- should not happen
          emptyContent

prettyPrintGoDelta :: Delta Label -> Array Content -> Content
prettyPrintGoDelta root children =
  case root of
    Same a ->
      prettyPrintGo a children
    Different ->
      case children of
        [left, right] ->
          noParens $
            noWrap (markRemoved left) <> noWrap (markAdded right)
        _ ->
          -- should not happen
          emptyContent
    Extra1 ->
      case children of
        [x] ->
          markRemoved x
        _ ->
          -- should not happen
          emptyContent
    Extra2 ->
      case children of
        [x] ->
          markAdded x
        _ ->
          -- should not happen
          emptyContent
    Subtree a ->
      prettyPrintGo a children

ansiGreen :: String
ansiGreen = "\27[32m"

ansiRed :: String
ansiRed = "\27[31m"

ansiReset :: String
ansiReset = "\27[0m"

markAdded :: Content -> Content
markAdded =
  surround (ansiGreen <> "+") ansiReset

markRemoved :: Content -> Content
markRemoved =
  surround (ansiRed <> "-") ansiReset

class Sized a where
  size :: a -> Int

instance sizedLabel :: Sized Label where
  size =
    case _ of
      Int _ ->
        1
      Number _ ->
        1
      Boolean _ ->
        1
      Char _ ->
        1
      String x ->
        if String.length x <= 15 then 1 else 2
      Array ->
        1
      Record ->
        2
      Prop name ->
        if String.length name <= 15 then 0 else 1
      App name ->
        if String.length name <= 15 then 1 else 2
      Opaque name ->
        if String.length name <= 15 then 1 else 2
      Collection name ->
        if String.length name <= 15 then 1 else 2
      Assoc _ ->
        2
      AssocProp ->
        0

instance sizedDelta :: Sized a => Sized (Delta a) where
  size =
    case _ of
      Same x ->
        size x
      Subtree x ->
        size x
      _ ->
        0

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

diff' :: forall a. Eq a => Tree a -> Tree a -> Tree (Delta a)
diff' = go
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

-- | Compare two `Repr` values and record the results as a `ReprDelta`
-- | structure.
diff :: Repr -> Repr -> ReprDelta
diff (Repr a) (Repr b) = ReprDelta (diff' a b)

-- | A delta between two `Repr` values; describes the differences between two
-- | values. Useful for testing, as this type can show you exactly where an
-- | expected and an actual value differ.
newtype ReprDelta = ReprDelta (Tree (Delta Label))

unReprDelta :: ReprDelta -> Tree (Delta Label)
unReprDelta (ReprDelta tree) = tree

derive newtype instance eqReprDelta :: Eq ReprDelta
derive newtype instance ordReprDelta :: Ord ReprDelta
