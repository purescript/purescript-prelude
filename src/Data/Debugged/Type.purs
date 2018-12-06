-- | This module provides the `Repr` type, which may be used as a common
-- | format for representations of values. The `Repr` type is intended for
-- | use in testing, debugging, and in the REPL, but not in production code.
module Data.Debugged.Type
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
  , prettyPrintOneLine
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.String as String

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
  -- children should all have DProp labels.
  | Record

  -- This node represents a property with the given name. It should only occur
  -- as a direct descendent of a DRecord- or DOpaque-labelled node.  A node
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
  -- label should only have DProp labels as immediate children.
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

-- | Pretty-print a representation on a single line.
prettyPrintOneLine :: Repr -> String
prettyPrintOneLine = go <<< unRepr
  where
  go tree =
    case rootLabel tree of
      Int x ->
        show x
      Number x ->
        show x
      Boolean x ->
        show x
      Char x ->
        show x
      String x ->
        show x
      App name ->
        case children tree of
          [] ->
            name
          args ->
            name <> " " <> String.joinWith " " (map prettyPrintAtom args)
      Array ->
        "[" <>
          String.joinWith ", " (map go (children tree)) <>
          "]"
      Record ->
        "{" <>
          String.joinWith ", " (map printProp (children tree)) <>
          "}"
      Opaque name ->
        case children tree of
          [] ->
            "<" <> name <> ">"
          xs ->
            "<" <> name <> " " <>
              String.joinWith ", " (map printProp xs)
              <> ">"
      Collection name ->
        "<" <> name <> " [" <>
          String.joinWith ", " (map go (children tree)) <>
          "]>"
      Assoc name ->
        "<" <> name <> " {" <>
          String.joinWith ", " (map printAssoc (children tree)) <>
          "}>"

      -- should not happen
      AssocProp -> ""
      Prop _ -> ""

  printProp :: Tree Label -> String
  printProp (Node (Prop name) [val]) =
    name <> ": " <> go val
  printProp _ =
    -- should not happen
    ""

  printAssoc :: Tree Label -> String
  printAssoc (Node AssocProp [a, b]) =
    go a <> ": " <> go b
  printAssoc _ =
    -- should not happen
    ""

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

-- | Pretty-print a representation, adding parens if necessary.
prettyPrintAtom :: Tree Label -> String
prettyPrintAtom d =
  if needsParens d
    then "(" <> prettyPrintOneLine (Repr d) <> ")"
    else prettyPrintOneLine (Repr d)

-------------------------------------------------------------------------------
-- DIFFING --------------------------------------------------------------------

-- | A type for labels for delta trees. If a tree has labels of type `a`, then
-- | we can represent a delta tree with labels of type `Delta a`.
data Delta a
  -- Each occurrence of this label corresponds to a node where the two trees
  -- being compared are identical.
  = Same a

  -- This label indicates that the two trees being compared differ at the node
  -- being labelled. Every node with this label should have precisely two
  -- children: the first being the subtree rooted here in the first of the two
  -- trees being diffed, and the second being the subtree rooted here in the
  -- second.
  | Different
