module Test.Utils where

import Prim.Row (class Union)

import Prelude

type AlmostEff = Unit -> Unit

assert :: String -> Boolean -> AlmostEff
assert msg condition = if condition then const unit else throwErr msg

foreign import throwErr :: String -> AlmostEff

-- Copied from https://github.com/purescript/purescript-record/blob/master/src/Record.purs#L182-L188
-- and renamed to `recordUnion`
recordUnion
  :: forall r1 r2 r3
   . Union r1 r2 r3
  => Record r1
  -> Record r2
  -> Record r3
recordUnion l r = unsafeUnion l r

foreign import unsafeUnion :: forall r1 r2 r3. Record r1 -> Record r2 -> Record r3
