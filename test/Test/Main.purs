module Test.Main where

import Prelude

type AlmostEff = Unit -> Unit

main :: AlmostEff
main = do
    testNumberShow show
    testOrderings
    testOrdUtils
    testIntDegree

foreign import testNumberShow :: (Number -> String) -> AlmostEff
foreign import throwErr :: String -> AlmostEff


assert :: String -> Boolean -> AlmostEff
assert msg condition = if condition then const unit else throwErr msg

testOrd :: forall a. Ord a => Show a => a -> a -> Ordering -> AlmostEff
testOrd x y ord =
    assert
        ("(compare " <> show x <> " " <> show y <> " ) is not equal to " <> show ord)
        $ (compare x y) == ord

nan :: Number
nan = 0.0/0.0

-- Unfortunately, NaN inhabits our Int
intNan :: Int
intNan = mod 1 0

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
    testOrd intNan 2147483647 GT
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

testIntDegree :: AlmostEff
testIntDegree = do
    let bot = bottom :: Int
    assert "degree returns absolute integers" $ degree (-4) == 4
    assert "degree returns absolute integers" $ degree 4 == 4
    assert "degree returns absolute integers" $ degree bot >= 0
    assert "degree does not return out-of-bounds integers" $ degree bot <= top
