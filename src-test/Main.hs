import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import Lib (evaluate, parse)

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" scTests
  , testGroup "Unit tests" huTests
  ]

scTests :: [TestTree]
scTests = []
  --[ testProperty "inc == succ" \n -> inc n == succ n
  --, testProperty "inc . negate == negate . pred" \n inc (negate n) == negate (pred n)
  --]

huTests :: [TestTree]
huTests =
  [ testCase "Add" case_add
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

fromRight :: Either a b -> b
fromRight (Right a) = a
fromRight _ = error "Data.Either.Utils.fromRight: Left"

case_add :: Assertion
case_add = evaluate (fromRight $ parse "1 + 4") @?= Just 5

case_subtract :: Assertion
case_subtract = evaluate (fromRight $ parse "10 - 3") @?= Just 7

case_multiply :: Assertion
case_multiply = evaluate (fromRight $ parse "3 * 4") @?= Just 12

case_divide :: Assertion
case_divide = evaluate (fromRight $ parse "16 / 4") @?= Just 4

case_factorial :: Assertion
case_factorial = evaluate (fromRight $ parse "5!") @?= Just 120

case_exp :: Assertion
case_exp = evaluate (fromRight $ parse "3 ^ 2") @?= Just 9

case_ln :: Assertion
case_ln = evaluate (fromRight $ parse "ln 1") @?= Just 0

case_log :: Assertion
case_log = evaluate (fromRight $ parse "log 100") @?= Just 2

case_modulus :: Assertion
case_modulus = evaluate (fromRight $ parse "13 % 5") @?= Just 3

case_negate :: Assertion
case_negate = evaluate (fromRight $ parse "-6") @?= Just (-6)

case_e :: Assertion
case_e = evaluate (fromRight $ parse "e^1") @?= Just 2.718281828459045
