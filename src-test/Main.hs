import Control.Monad (liftM2)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Lib (evaluate, parse, render, Expr(..))

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "QuickCheck" qcTests
  , testGroup "Unit tests" huTests
  ]

qcTests :: [TestTree]
qcTests =
  [ testProperty "id == parse . show" prop_parse_render
  ]

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

newtype Expression = Expression { getValue :: Expr Double }
instance Arbitrary Expression where
    arbitrary = sized expr
        where
          expr :: Int -> Gen Expression
          expr 0 = fmap (Expression . Number . abs) arbitrary
          expr n = oneof $ fmap (fmap Expression) [
              fmap (Number . abs) arbitrary
            , liftM2 Add (fmap getValue subexpr) (fmap getValue subexpr)
            , liftM2 Subtract (fmap getValue subexpr) (fmap getValue subexpr)
            , liftM2 Multiply (fmap getValue subexpr) (fmap getValue subexpr)
            , liftM2 Divide (fmap getValue subexpr) (fmap getValue subexpr)
            , liftM2 Modulus (fmap getValue subexpr) (fmap getValue subexpr)
            , liftM2 Exp (fmap getValue subexpr) (fmap getValue subexpr)
            , fmap (Logarithm (Number $ exp 1) . getValue) subexpr
            , fmap (Logarithm (Number 10) . getValue) subexpr
            , fmap (Factorial . getValue) subexpr
            , fmap (Negate . getValue) subexpr ]
              where subexpr = expr (n `div` 2)

instance Show Expression where
    show = show . getValue

prop_parse_render :: Expression -> Bool
prop_parse_render e = case parse (render (getValue e)) of
    Left _ -> False
    Right x -> x == getValue e
