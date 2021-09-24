module Data.Debug.Type.InternalRoseTree where

import Prelude

-------------------------------------------------------------------------------
-- BASIC DATA TYPES -----------------------------------------------------------

-- | A strict rose tree type. Based on Data.Tree in Haskell's `containers`.
-- | This type is intended to be used only by `Data.Debug.Type (Repr)`
-- | and pretty-printers that print the `Repr` values.
data InternalRoseTree a
  = Node a (Array (InternalRoseTree a))

rootLabel :: forall a. InternalRoseTree a -> a
rootLabel (Node r _) = r

subtrees :: forall a. InternalRoseTree a -> Array (InternalRoseTree a)
subtrees (Node _ xs) = xs

isLeaf :: forall a. InternalRoseTree a -> Boolean
isLeaf (Node _ []) = true
isLeaf _ = false

derive instance eqInternalRoseTree :: Eq a => Eq (InternalRoseTree a)
derive instance ordInternalRoseTree :: Ord a => Ord (InternalRoseTree a)
derive instance functorInternalRoseTree :: Functor InternalRoseTree

-- | Fold an InternalRoseTree bottom-up; the function `foldTree f` applies `f []` to each of
-- | the leaf nodes, and then works its way up the internal nodes, applying `f`
-- | to:
-- | - the label at the current node, and
-- | - the result of applying `f` to each child,
-- | finishing at the root.
foldTree :: forall a b. (a -> Array b -> b) -> InternalRoseTree a -> b
foldTree f = go
  where
  go (Node x ts) = f x (map go ts)

-- | Remove all children below a certain depth and replace them with a leaf
-- | with the given label. The arguments are:
-- | - the label to use for leaves which replace pruned subtrees.
-- | - a function which tells us whether to count the current node as
-- |   contributing to the depth of the subtree
-- | - the depth to preserve.
prune :: forall a. a -> (a -> Boolean) -> Int -> InternalRoseTree a -> InternalRoseTree a
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