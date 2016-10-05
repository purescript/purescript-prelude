module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd

data List a = Nil | Cons a (List a)

instance genericList :: G.Generic (List a)
                                  (G.Sum (G.Constructor "Nil" G.NoArguments)
                                         (G.Constructor "Cons" (G.Product (G.Argument a)
                                                                          (G.Argument (List a))))) where
  to (G.Inl (G.Constructor G.NoArguments)) = Nil
  to (G.Inr (G.Constructor (G.Product (G.Argument x) (G.Argument xs)))) = Cons x xs
  from Nil = G.Inl (G.Constructor G.NoArguments)
  from (Cons x xs) = G.Inr (G.Constructor (G.Product (G.Argument x) (G.Argument xs)))

instance eqList :: Eq a => Eq (List a) where
  eq x y = GEq.genericEq x y

instance ordList :: Ord a => Ord (List a) where
  compare x y = GOrd.genericCompare x y

main :: Eff (console :: CONSOLE) Unit
main = do
  logShow (Cons 1 (Cons 2 Nil) == Cons 1 (Cons 2 Nil))
  logShow (Cons 1 (Cons 2 Nil) == Cons 1 Nil)

  logShow (Cons 1 (Cons 2 Nil) `compare` Cons 1 (Cons 2 Nil))
  logShow (Cons 1 (Cons 2 Nil) `compare` Cons 1 Nil)
