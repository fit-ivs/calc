import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Lib (inc)

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests = 
  [ testGroup "SmallCheck" scTests
  , testGroup "Unit tests" huTests

    


  ]

scTests :: [TestTree]
scTests =
  [ testProperty "inc == succ" prop_succ
  , testProperty "inc . negate == negate . pred" prop_pred
  ]

huTests :: [TestTree]
huTests =
  [ testCase "Increment below TheAnswer" case_inc_below
  , testCase "Decrement above TheAnswer" case_dec_above
  , testCase "Add" case_add
  , testCase "Subtract" case_subtract
  , testCase "Multiply" case_multiply
  , testCase "Divide" case_divide
  , testCase "Factorial" case_factorial
  , testCase "Exp" case_exp
  , testCase "Ln" case_ln
  , testCase "Log" case_log
  , testCase "Modulus" case_modulus
  , testCase "Negate" case_negate
  , testCase "e" case_e
  ]





prop_succ :: Int -> Bool
prop_succ n = inc n == succ n

prop_pred :: Int -> Bool
prop_pred n = inc (negate n) == negate (pred n)

case_inc_below :: Assertion
case_inc_below = inc 41 @?= (42 :: Int)

case_dec_above :: Assertion
case_dec_above = negate (inc (negate 43)) @?= (42 :: Int)

case_add :: Assertion
case_add = evaluate (parse "1 + 4") @?= 5

case_subtract :: Assertion
case_subtract = evaluate (parse "10 - 3") @?= 7

case_multiply :: Assertion
case_multiply = evaluate (parse "3 * 4") @?= 12

case_divide :: Assertion
case_divide = evaluate (parse "16 / 4") @?= 4

case_factorial :: Assertion
case_factorial = evaluate (parse "5!") @?= 120

case_exp :: Assertion
case_exp = evaluate (parse "3 ^ 2") @?= 9

case_ln :: Assertion
case_ln = evaluate (parse "ln 1") @?= 0

case_log :: Assertion
case_log = evaluate (parse "log 100") @?= 2

case_modulus :: Assertion
case_modulus = evaluate (parse "13 % 5") @?= 3

case_negate :: Assertion
case_negate = evaluate (parse "-6") @?= -6

case_e :: Assertion
case_e = evaluate (parse "e^1.0986122886666") @?= 3
