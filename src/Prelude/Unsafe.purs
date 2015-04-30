module Prelude.Unsafe where

-- | Find the element of an array at the specified index.
-- |
-- | Note: this function can cause unpredictable failure at runtime if the index is out-of-bounds.
foreign import unsafeIndex
  """
  function unsafeIndex(xs) {
    return function(n) {
      return xs[n];
    };
  }
  """ :: forall a. Array a -> Number -> a
