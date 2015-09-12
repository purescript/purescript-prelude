module Data.Eq.Unsafe where

foreign import nativeEq :: forall a. a -> a -> Boolean
