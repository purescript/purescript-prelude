module Prelude
  ( module P
  , otherwise
  ) where

import qualified Control.Applicative as P
import qualified Control.Apply as P
import qualified Control.Bind as P
import qualified Control.Monad as P
import qualified Data.BooleanAlgebra as P
import qualified Data.Bounded as P
import qualified Data.BoundedOrd as P
import qualified Data.Category as P
import qualified Data.DivisionRing as P
import qualified Data.Eq as P
import qualified Data.Function (($), (#), flip, const) as P
import qualified Data.Functor as P
import qualified Data.ModuloSemiring as P
import qualified Data.Num as P
import qualified Data.Ord as P
import qualified Data.Ring as P
import qualified Data.Semigroup as P
import qualified Data.Semigroupoid as P
import qualified Data.Semiring as P
import qualified Data.Show as P
import qualified Data.Unit as P

-- | An alias for `true`, which can be useful in guard clauses:
-- |
-- | ```purescript
-- | max x y | x >= y    = x
-- |         | otherwise = y
-- | ```
otherwise :: Boolean
otherwise = true
