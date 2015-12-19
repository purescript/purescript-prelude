module Data.Void where

import Data.Show (class Show)

newtype Void = Void Void

instance showVoid :: Show Void where
  show _ = "Void"

absurd :: forall a. Void -> a
absurd a = spin a
  where
  spin (Void b) = spin b
