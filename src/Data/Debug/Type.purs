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
  , opaque_
  , opaqueLiteral
  , collection
  , assoc

  -- diffing
  , ReprDelta
  , diffRepr
  , diffReprWith
  , DiffOptions
  , defaultDiffOptions

  -- pretty printing
  , prettyPrint
  , prettyPrintDelta
  , prettyPrintWith
  , prettyPrintDeltaWith
  , PrettyPrintOptions
  , defaultPrettyPrintOptions
  ) where

import Prelude

import Data.Array as Array
import Data.Debug.PrettyPrinter (Content, commaSeq, compact, emptyContent, indent, leaf, noParens, noWrap, parens, printContent, surround, verbatim, wrap)
import Data.Foldable (foldMap, all, elem)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Math as Math

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

-- | Remove all children below a certain depth and replace them with a leaf
-- | with the given label. The arguments are:
-- | - the label to use for leaves which replace pruned subtrees.
-- | - a function which tells us whether to count the current node as
-- |   contributing to the depth of the subtree
-- | - the depth to preserve.
prune :: forall a. a -> (a -> Boolean) -> Int -> Tree a -> Tree a
prune replacement counts depth = go (max 1 depth)
  where
  -- If we've reached a leaf anyway, just print it
  go 0 n@(Node _ []) =
    n
  go 0 _ =
    Node replacement []
  go d (Node label children) =
    let
      d' = if counts label then d-1 else d
    in
      Node label (map (go d') children)

-------------------------------------------------------------------------------
-- THE REPR TYPE & CONSTRUCTORS -----------------------------------------------

-- | A debugging representation of some PureScript value. It is often possible
-- | to reconstruct the original value from this representation, but not
-- | always; notable counterexamples are `Effect`, `Ref`, and `(->)`.
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

makeProps :: Array { key :: String, value :: Repr } -> Array (Tree Label)
makeProps = map unwrapProp
  where
  unwrapProp :: { key :: String, value :: Repr } -> Tree Label
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
  makeAssocProp :: { key :: Repr, value :: Repr } -> Tree Label
  makeAssocProp { key: Repr k, value: Repr v } = Node AssocProp [k, v]

-- | Should a label be considered as adding depth (from the perspective of
-- | only pretty-printing to a certain depth)?
addsDepth :: Label -> Boolean
addsDepth =
  case _ of
    Prop _ -> false
    AssocProp -> false
    _ -> true

relativeError :: Number -> Number -> Number
relativeError x y = Math.abs (x - y) / max (Math.abs x) (Math.abs y)

eqRelative :: Number -> Number -> Number -> Boolean
eqRelative error x y = relativeError x y <= error

-- | Compare labels for approximate equality (in the case of Number
-- | constructors), using the specified relative error.
labelApproxEq :: Number -> Label -> Label -> Boolean
labelApproxEq error x y =
  case x, y of
    Number x', Number y' ->
      eqRelative error x' y'
    _, _ ->
      x == y

labelIsUnimportant :: Label -> Boolean
labelIsUnimportant =
  case _ of
    AssocProp ->
      true
    Array ->
      true
    Record ->
      true
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

-- | Options for diffing `Repr` values.
-- |
-- | - **maxRelativeError:** The maximum relative error within which two
-- |   `Number` values may be considered equal.
type DiffOptions =
  { maxRelativeError :: Number }

defaultDiffOptions :: DiffOptions
defaultDiffOptions =
  { maxRelativeError: 1e-12 }

diff' :: forall a.
  (a -> a -> Boolean) ->
  (a -> Boolean) ->
  Tree a ->
  Tree a ->
  Tree (Delta a)
diff' labelEq isUnimportantLabel = go
  where
  go left@(Node x xs) right@(Node y ys) =
    if labelEq x y
      then
        let
          children = goChildren xs ys
        in
          if isUnimportantLabel x && all differing children
            then
              Node Different [map Subtree left, map Subtree right]
            else
              Node (Same x) children
      else
        Node Different [map Subtree left, map Subtree right]

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

  differing :: Tree (Delta a) -> Boolean
  differing (Node root _) =
    case root of
      Same _ ->
        false
      _ ->
        true

-- | Compare two `Repr` values and record the results as a `ReprDelta`
-- | structure, using the specified options.
diffReprWith :: DiffOptions -> Repr -> Repr -> ReprDelta
diffReprWith opts (Repr a) (Repr b) =
  ReprDelta $
    diff'
      (labelApproxEq opts.maxRelativeError)
      labelIsUnimportant
      a
      b

-- | Compare two `Repr` values and record the results as a `ReprDelta`
-- | structure, using the default options.
diffRepr :: Repr -> Repr -> ReprDelta
diffRepr = diffReprWith defaultDiffOptions

-- | A delta between two `Repr` values; describes the differences between two
-- | values. Useful for testing, as this type can show you exactly where an
-- | expected and an actual value differ.
newtype ReprDelta = ReprDelta (Tree (Delta Label))

unReprDelta :: ReprDelta -> Tree (Delta Label)
unReprDelta (ReprDelta tree) = tree

derive newtype instance eqReprDelta :: Eq ReprDelta
derive newtype instance ordReprDelta :: Ord ReprDelta

-- | Should a label be considered as adding depth from the perspective of
-- | only pretty-printing to a certain depth?
addsDepthDelta :: forall a. (a -> Boolean) -> Delta a -> Boolean
addsDepthDelta f =
  case _ of
    Subtree label ->
      f label
    Same label ->
      f label
    _ ->
      false

-------------------------------------------------------------------------------
-- PRETTY-PRINTING ------------------------------------------------------------

-- | Options for configuring the pretty-printer.
-- |
-- | - **maxDepth:** How many levels of the tree should be printed before
-- |   cutting off. If set to `Nothing`, the entire tree is always printed.
-- | - **compactThreshold:** Controls how large a subtree is allowed to become
-- |   before it is broken over multiple lines. The larger this value is, the
-- |   fewer lines will be needed to pretty-print something.
type PrettyPrintOptions
  = { maxDepth :: Maybe Int
    , compactThreshold :: Int
    }

defaultPrettyPrintOptions :: PrettyPrintOptions
defaultPrettyPrintOptions =
  { maxDepth: Just 4
  , compactThreshold: 8
  }

-- | Pretty-print a `Repr` value using the specified options; intended for use
-- | in e.g. the repl.
-- |
-- | The output will be executable PureScript code provided that the given
-- | `Repr` value does not contain any nodes which were constructed with the
-- | `opaque`, `collection`, or `assoc` functions.
prettyPrintWith :: PrettyPrintOptions -> Repr -> String
prettyPrintWith opts =
  printContent
  <<< foldTree (withResizing labelSize opts.compactThreshold prettyPrintGo)
  <<< pruneTo opts.maxDepth
  <<< unRepr

  where
  pruneTo = maybe identity (prune Omitted addsDepth)

-- | Pretty-print a `Repr` value using the default options; see also
-- | `prettyPrintWith`.
prettyPrint :: Repr -> String
prettyPrint = prettyPrintWith defaultPrettyPrintOptions

-- | Pretty-print a `ReprDelta` value using the specified options.
-- |
-- | The result will contain ANSI terminal codes to mark additions in green
-- | and deletions in red. A value is considered to have been 'added' if it
-- | exists in the second argument to `diff` but not the first, and similarly
-- | it is considered 'deleted' if it appears in the first but not the second.
prettyPrintDeltaWith :: PrettyPrintOptions -> ReprDelta -> String
prettyPrintDeltaWith opts =
  printContent
  <<< foldTree (withResizing (deltaSize labelSize)
                             opts.compactThreshold
                             prettyPrintGoDelta)
  <<< pruneTo opts.maxDepth
  <<< unReprDelta

  where
  pruneTo = maybe identity (prune (Same Omitted) (addsDepthDelta addsDepth))

-- | Pretty-print a `ReprDelta` value using the default options; see also
-- | `prettyPrintDeltaWith`.
prettyPrintDelta :: ReprDelta -> String
prettyPrintDelta = prettyPrintDeltaWith defaultPrettyPrintOptions

measure :: forall a. (a -> Int) -> a -> Array Content -> Int
measure size root children =
  size root + unwrap (foldMap _.size children)

withResizing :: forall a.
  (a -> Int) ->
  Int ->
  (a -> Array Content -> Content) ->
  (a -> Array Content -> Content)
withResizing size threshold f root children =
  if measure size root children <= threshold
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
      let
        f = if Array.length children > 0 then parens else noParens
      in
        f (verbatim name <> foldMap (indent "  " <<< wrap) children)
    Array ->
      commaSeq "[ " " ]" children
    Record ->
      commaSeq "{ " " }" children
    Opaque name ->
      if Array.null children
        then
          noParens $ surround "<" ">" $ verbatim name
        else
          noParens $
            surround "<" ">" $
              verbatim (name <> ":")
              <> indent "  " (noWrap (commaSeq "" "" children))
    Literal str ->
      noParens $ verbatim str
    Assoc name ->
      noParens $
        surround "<" ">" $
          verbatim (name <> ":") <> noWrap (commaSeq "{ " " }" children)
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
          noParens $
            verbatim (prettyPrintLabel name <> ":")
            <> indent "  " (noWrap val)
        _ ->
          -- should not happen
          emptyContent
    Omitted ->
      parens (verbatim "...")

prettyPrintLabel :: String -> String
prettyPrintLabel name =
  if isUnquotedKey name
    then name
    else show name

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
ansiGreen = "\x1b[32m"

ansiRed :: String
ansiRed = "\x1b[31m"

ansiReset :: String
ansiReset = "\x1b[0m"

markAdded :: Content -> Content
markAdded =
  surround (ansiGreen <> "+") ansiReset

markRemoved :: Content -> Content
markRemoved =
  surround (ansiRed <> "-") ansiReset

labelSize :: Label -> Int
labelSize =
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
    Literal str ->
      if String.length str <= 15 then 1 else 2
    Assoc _ ->
      2
    AssocProp ->
      0
    Omitted ->
      0

deltaSize :: forall a. (a -> Int) -> Delta a -> Int
deltaSize size =
  case _ of
    Same x ->
      size x
    Subtree x ->
      size x
    _ ->
      0

-- | Check whether a record key would need to be quoted if it were appearing
-- | in a record literal in a PureScript source file. Note that currently this
-- | function is more conservative than it needs to be.
-- |
-- | See `Language.PureScript.Parser.Lexer.isUnquotedKey`.
isUnquotedKey :: String -> Boolean
isUnquotedKey key =
  case String.uncons key of
    Nothing ->
      false
    Just { head, tail } ->
      isUnquotedKeyHead head
      && all isUnquotedKeyTail (String.toCodePointArray tail)

-- | Note that this is more restrictive than necessary, since we consider
-- | record labels beginning with a lowercase non-ascii character to require
-- | quoting, when in fact this is not necessarily true.
isUnquotedKeyHead :: String.CodePoint -> Boolean
isUnquotedKeyHead = ((_ == "_") || isLowerAlphaAscii) <<< String.singleton

isLowerAlphaAscii :: String -> Boolean
isLowerAlphaAscii = between "a" "z"

isUpperAlphaAscii :: String -> Boolean
isUpperAlphaAscii = between "A" "Z"

isNumericAscii :: String -> Boolean
isNumericAscii = between "0" "9"

-- | Note that this is more restrictive than necessary, since we consider
-- | record label tails containing non-ascii characters to require quoting when
-- | in fact this is not necessarily true.
isUnquotedKeyTail :: String.CodePoint -> Boolean
isUnquotedKeyTail =
  (_ `elem` (map String.codePointFromChar ['_', '\'']))
    || (isAlphaNumAscii <<< String.singleton)

isAlphaNumAscii :: String -> Boolean
isAlphaNumAscii = isLowerAlphaAscii || isUpperAlphaAscii || isNumericAscii
