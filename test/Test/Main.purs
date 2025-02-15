module Test.Main where

import Prelude
import Data.HeytingAlgebra (ff, tt, implies)
import Data.Ord (abs, signum)
import Data.Reflectable (reflectType, reifyType)
import Prim.Boolean (True, False)
import Prim.Ordering (LT, GT, EQ)
import Test.Data.Generic.Rep (testGenericRep)
import Test.Utils (AlmostEff, assert)
import Type.Proxy (Proxy(..))

main :: AlmostEff
main = do
    testNumberShow show
    testOrderings
    testOrdUtils
    testIntDivMod
    testIntDegree
    testRecordInstances
    testGenericRep
    testReflectType
    testReifyType
    testSignum
    testArrayBind

foreign import testNumberShow :: (Number -> String) -> AlmostEff

testOrd :: forall a. Ord a => Show a => a -> a -> Ordering -> AlmostEff
testOrd x y ord =
    assert
        ("(compare " <> show x <> " " <> show y <> " ) is not equal to " <> show ord)
        $ (compare x y) == ord

nan :: Number
nan = 0.0/0.0

plusInfinity :: Number
plusInfinity = 1.0/0.0

minusInfinity :: Number
minusInfinity = -1.0/0.0

testOrderings :: AlmostEff
testOrderings = do
    assert "NaN shouldn't be equal to itself" $ nan /= nan
    assert "NaN shouldn't be equal to itself" $ (compare nan nan) /= EQ
    testOrd 1.0    2.0 LT
    testOrd 2.0    1.0 GT
    testOrd 1.0    (-2.0) GT
    testOrd (-2.0) 1.0 LT
    testOrd minusInfinity plusInfinity LT
    testOrd minusInfinity 0.0 LT
    testOrd plusInfinity  0.0 GT
    testOrd plusInfinity  minusInfinity GT
    testOrd 1.0 nan GT
    testOrd nan 1.0 GT
    testOrd nan plusInfinity GT
    testOrd plusInfinity nan GT
    assert "1 > NaN should be false" $ (1.0 > nan) == false
    assert "1 < NaN should be false" $ (1.0 < nan) == false
    assert "NaN > 1 should be false" $ (nan > 1.0) == false
    assert "NaN < 1 should be false" $ (nan < 1.0) == false
    assert "NaN == 1 should be false" $ nan /= 1.0
    testOrd (1 / 0) 0 EQ
    testOrd (mod 1 0) 0 EQ
    testOrd 'a' 'b' LT
    testOrd 'b' 'A' GT
    testOrd "10" "0" GT
    testOrd "10" "2" LT
    testOrd true  true EQ
    testOrd false false EQ
    testOrd false true LT
    testOrd true  false GT
    testOrd ([] :: Array Int) [] EQ
    testOrd [1, 0]  [1] GT
    testOrd [1]     [1, 0] LT
    testOrd [1, 1]  [1, 0] GT
    testOrd [1, -1] [1, 0] LT

testOrdUtils :: AlmostEff
testOrdUtils = do
  assert "-5 clamped between 0 and 10 should be 0" $ clamp 0 10 (-5) == 0
  assert "5 clamped between 0 and 10 should be 5" $ clamp 0 10 5 == 5
  assert "15 clamped between 0 and 10 should be 10" $ clamp 0 10 15 == 10
  assert "-5 should not be between 0 and 10" $ between 0 10 (-5) == false
  assert "5 should be between 0 and 10" $ between 0 10 5 == true
  assert "15 should not be between 0 10" $ between 0 10 15 == false

testIntDivMod :: AlmostEff
testIntDivMod = do
  -- Check when dividend goes into divisor exactly
  go 8 2
  go (-8) 2
  go 8 (-2)
  go (-8) (-2)

  -- Check when dividend does not go into divisor exactly
  go 2 3
  go (-2) 3
  go 2 (-3)
  go (-2) (-3)

  where
  go a b =
    let
      q = a / b
      r = a `mod` b
      msg = show a <> " / " <> show b <> ": "
    in do
      assert (msg <> "Quotient/remainder law") $
        q * b + r == a
      assert (msg <> "Remainder should be between 0 and `abs b`, got: " <> show r) $
        0 <= r && r < abs b

testIntDegree :: AlmostEff
testIntDegree = do
    let bot = bottom :: Int
    assert "degree returns absolute integers" $ degree (-4) == 4
    assert "degree returns absolute integers" $ degree 4 == 4
    assert "degree returns absolute integers" $ degree bot >= 0
    assert "degree does not return out-of-bounds integers" $ degree bot <= top

testRecordInstances :: AlmostEff
testRecordInstances = do
  assert "Record equality" $ { a: 1 } == { a: 1 }
  assert "Record inequality" $ { a: 2 } /= { a: 1 }
  assert "Record show nil" $ show { } == "{}"
  assert "Record show one" $ show { a: 1 } == "{ a: 1 }"
  assert "Record show more" $ show { a: 1, b: 2 } == "{ a: 1, b: 2 }"
  assert "Record +" $ ({ a: 1, b: 2.0 } + { a: 0, b: (-2.0) }) == { a: 1, b: 0.0 }
  assert "Record *" $ ({ a: 1, b: 2.0 } * { a: 0, b: (-2.0) }) == { a: 0, b: -4.0 }
  assert "Record one" $ one == { a: 1, b: 1.0 }
  assert "Record zero" $ zero == { a: 0, b: 0.0 }
  assert "Record sub" $ { a: 2, b: 2.0 } - { a: 1, b: 1.0 } == { a: 1, b: 1.0 }
  assert "Record append" $ { a: [], b: "T" } <> { a: [1], b: "OM" } == { a: [1], b: "TOM" }
  assert "Record mempty" $ mempty == { a: [] :: Array Int, b: "" }
  assert "Record ff" $ ff == { a: false, b: false }
  assert "Record tt" $ tt == { a: true, b: true }
  assert "Record not" $ not { a: true, b: false } == { a: false, b: true }
  assert "Record conj" $ conj
    { a: true, b: false, c: true, d: false }
    { a: true, b: true, c: false, d: false }
    == { a: true, b: false, c: false, d: false }
  assert "Record disj" $ disj
    { a: true, b: false, c: true, d: false }
    { a: true, b: true, c: false, d: false }
    == { a: true, b: true, c: true, d: false }
  assert "Record implies" $ implies
    { a: true, b: false, c: true, d: false }
    { a: true, b: true, c: false, d: false }
    == { a: true, b: true, c: false, d: true }
  testOrd { a: 0, b: "hello" } { a: 42, b: "hello" } LT
  testOrd { a: 42, b: "hello" } { a: 0, b: "hello" } GT
  testOrd { a: 42, b: "hello" } { a: 42, b: "hello" } EQ
  testOrd { a: 42, b: "hell" } { a: 42, b: "hello" } LT
  testOrd { a: 42, b: "hello" } { a: 42, b: "hell" } GT
  assert "Record bottom" $
    (bottom :: { a :: Boolean }).a
    == bottom
  assert "Record top" $
    (top :: { a :: Boolean }).a
    == top

testReflectType :: AlmostEff
testReflectType = do
  assert "reflectType: Symbol -> String" $ reflectType (Proxy :: _ "erin!") == "erin!"
  assert "reflectType: Boolean -> Boolean, True" $ reflectType (Proxy :: _ True) == true
  assert "reflectType: Boolean -> Boolean, False" $ reflectType (Proxy :: _ False) == false
  assert "reflectType: Ordering -> Ordering, LT" $ reflectType (Proxy :: _ LT) == LT
  assert "reflectType: Ordering -> Ordering, GT" $ reflectType (Proxy :: _ GT) == GT
  assert "reflectType: Ordering -> Ordering, EQ" $ reflectType (Proxy :: _ EQ) == EQ
  assert "reflectType: Int -> Int, 42" $ reflectType (Proxy :: _ 42) == 42
  assert "reflectType: Int -> Int, -42" $ reflectType (Proxy :: _ (-42)) == -42

testReifyType :: AlmostEff
testReifyType = do
  assert "reifyType: String -> Symbol" $ reifyType "erin!" reflectType == "erin!"
  assert "reifyType: Boolean -> Boolean, true" $ reifyType true reflectType == true
  assert "reifyType: Boolean -> Boolean, false" $ reifyType false reflectType == false
  assert "reifyType: Ordering -> Ordering, LT" $ reifyType LT reflectType == LT
  assert "reifyType: Ordering -> Ordering, GT" $ reifyType GT reflectType == GT
  assert "reifyType: Ordering -> Ordering, EQ" $ reifyType EQ reflectType == EQ
  assert "reifyType: Int -> Int, 42" $ reifyType 42 reflectType == 42
  assert "reifyType: Int -> Int, -42" $ reifyType (-42) reflectType == -42

testSignum :: AlmostEff
testSignum = do
  assert "Clarifies what 'signum positive zero' test is doing" $ show (1.0/0.0) == "Infinity"
  assert "signum positive zero" $ show (1.0/(signum 0.0)) == "Infinity"
  assert "Clarifies what 'signum negative zero' test is doing" $ show (1.0/(-0.0)) == "-Infinity"
  assert "signum negative zero" $ show (1.0/(signum (-0.0))) == "-Infinity"

foreign import makeArray :: Int -> Array Int

testArrayBind :: AlmostEff
testArrayBind = do
  assert "Array bind does not cause RangeError" do
    let
      _ = do
        _ <- [unit]
        makeArray 106_000
    true
