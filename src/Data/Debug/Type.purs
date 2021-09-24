-- | This module provides the basic building blocks which the `Debug` type
-- | class makes use of. It provides the `Repr` type and functions for
-- | constructing values of this type. It does not provide
-- | values for representing differences between two `Repr` values,
-- | nor pretty-printing functions.
module Data.Debug.Type
  -- Data type and construction
  ( Label(..)
  , Repr
  , int
  , number
  , boolean
  , char
  , string
  , array
  , record
  , constructor
  , opaque
  , opaque_
  , opaqueLiteral
  , collection
  , assoc
  , unRepr
  ) where

import Prelude

import Data.Debug.Type.InternalRoseTree (InternalRoseTree(..))

-------------------------------------------------------------------------------
-- THE REPR TYPE & CONSTRUCTORS -----------------------------------------------

-- | A debugging representation of some PureScript value. It is often possible
-- | to reconstruct the original value from this representation, but not
-- | always; notable counterexamples are `Effect`, `Ref`, and `(->)`.
newtype Repr = Repr (InternalRoseTree Label)

derive newtype instance eqRepr :: Eq Repr
derive newtype instance ordRepr :: Ord Repr

unRepr :: Repr -> InternalRoseTree Label
unRepr (Repr tree) = tree

-- | Labels for an InternalRoseTree which encodes a debugging representation of some
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
  -- label may have a number of Prop labels as immediate children, or
  -- alternatively a single Literal label.
  | Opaque String

  -- This constructor should only ever appear as an immediate child of an
  -- `Opaque` constructor, and should always be a leaf node. It is useful for
  -- types which have an obvious representation which doesn't fit into any of
  -- the other options for constructing Repr values. For example, dates or
  -- times may want to use this constructor in order to display as e.g. <Date:
  -- 2019-02-01> or <Time: 12:00? respectively.
  | Literal String

  -- This constructor is for map-like collections. The children are the
  -- contents; every direct child should have an AssocProp label.
  | Assoc String

  -- This constructor is for the key-value pairs of map-like collections. Each
  -- node with this label should have exactly two children: the key and the
  -- value.
  | AssocProp

  -- This constructor is only used for pretty-printing, where we have cut the
  -- tree off after a certain depth.
  | Omitted

derive instance eqLabel :: Eq Label
derive instance ordLabel :: Ord Label

mkLeaf :: Label -> Repr
mkLeaf label = Repr (Node label [])

-- | Create a `Repr` for an `Int`. This function should only be used to
-- | construct `Repr` values for values of the type `Int`; other numeric types
-- | should use one of the other functions available, such as `opaque`.
int :: Int -> Repr
int = mkLeaf <<< Int

-- | Create a `Repr` for an `Number`. This function should only be used to
-- | construct `Repr` values for values of the type `Number`; other numeric
-- | types should use one of the other functions available, such as `opaque`.
number :: Number -> Repr
number = mkLeaf <<< Number

-- | Create a `Repr` for an `Boolean`. This function should only be used to
-- | construct `Repr` values for values of the type `Boolean`.
boolean :: Boolean -> Repr
boolean = mkLeaf <<< Boolean

-- | Create a `Repr` for a `Char`. This function should only be used to
-- | construct `Repr` values for values of the type `Char`.
char :: Char -> Repr
char = mkLeaf <<< Char

-- | Create a `Repr` for a `String`. This function should only be used to
-- | construct `Repr` values for values of the type `String`.
string :: String -> Repr
string = mkLeaf <<< String

-- | Create a `Repr` for an `Array`, given its contents. This function should
-- | only be used to construct `Repr` values for values of the type `Array a`;
-- | other array-like types may use the `collection` function.
array :: Array Repr -> Repr
array = Repr <<< Node Array <<< map unRepr

-- | Create a `Repr` for a `Record`, given its fields. This function should
-- | only be used to construct `Repr` values for values of the type `Record r`;
-- | other record-like types may use the `assoc` function.
record :: Array { key :: String, value :: Repr } -> Repr
record =
  Repr <<< Node Record <<< makeProps

makeProps :: Array { key :: String, value :: Repr } -> Array (InternalRoseTree Label)
makeProps = map unwrapProp
  where
  unwrapProp :: { key :: String, value :: Repr } -> InternalRoseTree Label
  unwrapProp { key: name, value: Repr val } =
    Node (Prop name) [val]

-- | Create a `Repr` for a value constructed by a data constructor. For
-- | example, the value `Just 3` may be represented by `constructor "Just" [int
-- | 3]`.
constructor :: String -> Array Repr -> Repr
constructor name args =
  Repr (Node (App name) (map unRepr args))

-- | Create a `Repr` for an opaque type, such as `Ref` or `(->)`. The first
-- | argument is the type name, and the second may contain any additional
-- | information which might be useful to include in a pretty-printed
-- | representation.
opaque :: String -> Repr -> Repr
opaque name child =
  Repr (Node (Opaque name) [unRepr child])

-- | Like `opaque`, but without the second `Repr` argument; for when there is
-- | no additional information to provide.
opaque_ :: String -> Repr
opaque_ name =
  Repr (Node (Opaque name) [])

-- | Create a `Repr` for an opaque type. The first argument is the type name,
-- | and the second argument is a string representation of the value. This
-- | function should only be used in cases where no other constructor is
-- | appropriate, as `Repr` values constsructed this way will generally not be
-- | able to give detailed diffs.
opaqueLiteral :: String -> String -> Repr
opaqueLiteral name val =
  Repr (Node (Opaque name) [Node (Literal val) []])

-- | Create a `Repr` for a collection type. The first argument is the type
-- | name, the second is the contents. Defined as `\name contents -> opaque
-- | name (array contents)`.
collection :: String -> Array Repr -> Repr
collection name contents =
  opaque name (array contents)

-- | Create a `Repr` for a type representing a mapping of keys to values, such
-- | as `Map`. The first argument is the type name, the second is the contents.
assoc :: String -> Array { key :: Repr, value :: Repr } -> Repr
assoc name contents =
  Repr (Node (Assoc name) (map makeAssocProp contents))
  where
  makeAssocProp :: { key :: Repr, value :: Repr } -> InternalRoseTree Label
  makeAssocProp { key: Repr k, value: Repr v } = Node AssocProp [k, v]
