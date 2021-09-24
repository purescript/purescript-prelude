-- | A simple pretty-printer, intended for use with Debug.Repr values.
module Data.Debug.PrettyPrinter.PrettyPrinter where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Monoid.Additive (Additive(..))
import Data.String as String
import Partial.Unsafe (unsafePartial)

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

-- | An intermediate type used while folding a tree for pretty-printing.
type AnyContent r =
    -- The size of the tree so far; controls whether a tree is printed over
    -- multiple lines or not.
  { size :: Additive Int 
  -- The pretty-printed content. Each element in the array corresponds to one
  -- line in the eventual output.
  , lines :: Array String
  | r
  }

type Content = AnyContent
  -- If this content is eventually displayed in a context where it might need
  -- parentheses, such as in an argument to a data constructor, should those
  -- parentheses be included?
  ( needsParens :: Boolean )

-- | A variant of `Content` which is guaranteed to be wrapped in parens if they
-- | are going to be necessary. The `needsParens` field has therefore been
-- | dropped. Note that this type has a Monoid instance, whereas `Content` does
-- | not (because of the extra Boolean field); this is on purpose, since we can
-- | only safely concatenate `Content` values once we know they have been
-- | parenthesised where necessary.
type ContentParens = AnyContent ()

-- | Wrap a `Content` in parens. To be used in contexts where parens might be
-- | needed.
wrap :: Content -> ContentParens
wrap c =
  { size: c.size
  , lines: if c.needsParens then surroundLines "(" ")" c.lines else c.lines
  }

-- | Mark a `Content` as being in a context where parens are not needed.
noWrap :: forall r. AnyContent r -> ContentParens
noWrap c =
  { size: c.size
  , lines: c.lines
  }

unParens :: forall r. Boolean -> AnyContent r -> Content
unParens needsParens r =
  { size: r.size
  , lines: r.lines
  , needsParens
  }

-- | Turn a `ContentParens` back into a `Content`; for use in contexts where
-- | parens might be needed.
parens :: forall r. AnyContent r -> Content
parens = unParens true

-- | Turn a `ContentParens` back into a `Content`; for use in contexts where
-- | parens will not be needed.
noParens :: forall r. AnyContent r -> Content
noParens = unParens false

emptyContent :: Content
emptyContent =
  { size: Additive 0
  , lines: []
  , needsParens: false
  }

withLines :: forall r .
  (Array String -> Array String) ->
  AnyContent r -> AnyContent r
withLines f rec =
  rec { lines = f rec.lines }

-- | Reduce a multiple-line pretty-printed expression down to one line.
compactLines :: Array String -> Array String
compactLines =
  firstMiddleLast
  >>> case _ of
        Empty ->
          []
        Single x ->
          [x]
        TwoOrMore first middle last ->
          [String.joinWith " "
            ([first] <> map String.trim middle <> [String.trim last])]

compact :: forall r. AnyContent r -> AnyContent r
compact = withLines compactLines

surroundLines :: String -> String -> Array String -> Array String
surroundLines start finish =
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

surround :: forall r. String -> String -> AnyContent r -> AnyContent r
surround start finish = withLines (surroundLines start finish)

printContent :: forall r. AnyContent r -> String
printContent = String.joinWith "\n" <<< _.lines

-- | Create a `Content` from the label of a leaf node, using the label's `Show`
-- | instance.
leaf :: forall a. Show a => a -> Content
leaf x =
  { lines: [show x]
  , size: Additive 1
  , needsParens: false
  }

verbatim :: String -> ContentParens
verbatim x =
  { lines: [x]
  , size: Additive 1
  }

indent :: forall r. String -> AnyContent r -> AnyContent r
indent prefix r =
  r { lines = map (prefix <> _) r.lines }

-- | Produce a comma separated sequence over multiple lines with the given
-- | beginning and ending string sequences.
commaSeq :: forall r. String -> String -> Array (AnyContent r) -> Content
commaSeq begin end contents =
  { size: foldMap _.size contents
  , lines: commaSeqLines begin end (map _.lines contents)
  , needsParens: false
  }

commaSeqLines :: String -> String -> Array (Array String) -> Array String
commaSeqLines begin end =
  firstMiddleLast >>> go >>> Array.filter (_ /= "")
  where
  go =
    case _ of
      Empty ->
        [ begin <> end ]
      Single item ->
        surroundLines begin end item
      TwoOrMore first middle last ->
        surroundLines begin "," first
        <> Array.concatMap (surroundLines spacer ",") middle
        <> surroundLines spacer end last

  spacer = fold (Array.replicate (String.length begin) " ")

withLast :: forall a. (a -> a) -> Array a -> Array a
withLast f xs =
  case Array.length xs of
    0 ->
      []
    n ->
      unsafePartial $
        Array.slice 0 (n-2) xs <> [f (Array.unsafeIndex xs (n-1))]
