-- |
-- Utilities for parsing, evaluating, and pretty-printing arithmetic expressions.
--
module Lib (
    -- * Types
    Expr(..),

    -- * Methods
    parse,
    evaluate,
    render
    ) where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Expr
       (Operator(Prefix, InfixL, InfixR, Postfix), makeExprParser)
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L
import Data.Fixed (mod')

data Expr a
    = Number a
    | Add (Expr a)
          (Expr a)
    | Subtract (Expr a)
               (Expr a)
    | Multiply (Expr a)
               (Expr a)
    | Divide (Expr a)
             (Expr a)
    | Modulus (Expr a)
              (Expr a)
    | Exp (Expr a)
          (Expr a)
    | Logarithm (Expr a)
                (Expr a)
    | Factorial (Expr a)
    | Negate (Expr a)
    deriving (Show,Eq)

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operators :: [[Operator Parser (Expr Double)]]
operators =
    [ [ Prefix (symbol "-" *> pure Negate)
      , Postfix (symbol "!" *> pure Factorial)]
    , [ InfixR (symbol "^" *> pure Exp)
      , Prefix (symbol "e" *> symbol "^" *> pure (Exp (Number $ exp 1)))]
    , [ InfixL (symbol "*" *> pure Multiply)
      , InfixL (symbol "/" *> pure Divide)
      , InfixL (symbol "%" *> pure Modulus)]
    , [ InfixL (symbol "+" *> pure Add)
      , InfixL (symbol "-" *> pure Subtract)]
    , [ Prefix (symbol "ln" *> pure (Logarithm (Number $ exp 1)))
      , Prefix (symbol "log" *> pure (Logarithm (Number 10)))]]

signedFloat :: Parser Double
signedFloat = L.signed sc $ either fromIntegral id <$> L.lexeme sc L.number

term :: Parser (Expr Double)
term = parens expression <|> Number <$> signedFloat

expression :: Parser (Expr Double)
expression = makeExprParser term operators

-- | Parse an arithmetic expression.
--
-- >>> parse "x"
-- Left 1:1:
-- unexpected 'x'
-- expecting '(', '+', '-', or number
-- >>> parse "1"
-- Right (Number 1.0)
parse
    :: String -> Either ParseError (Expr Double)
parse = runParser (expression <* eof) ""

-- | Evaluate an arithmetic expression.
--
-- >>> evaluate (Number 10)
-- Just 10.0
-- >>> evaluate (Divide (Number 1) (Number 0))
-- Nothing
evaluate
    :: RealFloat a
    => Expr a -> Maybe a
evaluate (Number x)      = Just x
evaluate (Negate x)      = negate <$> evaluate x
evaluate (Add x y)       = (+) <$> evaluate x <*> evaluate y
evaluate (Subtract x y)  = (-) <$> evaluate x <*> evaluate y
evaluate (Multiply x y)  = (*) <$> evaluate x <*> evaluate y
evaluate (Exp x y)       = (**) <$> evaluate x <*> evaluate y
evaluate (Logarithm x y) = logBase <$> evaluate x <*> evaluate y
evaluate (Modulus x y)   = mod' <$> evaluate x <*> evaluate y
evaluate (Divide x y) = do
    x' <- evaluate x
    y' <- evaluate y
    if y' == 0
        then Nothing
        else Just (x' / y')
evaluate (Factorial x) = do
    x' <- evaluate x
    -- If x' approximates an integer
    if x' == fromInteger (round x')
        then Just $ fromInteger $ product [1 .. (round x' :: Integer)]
        else Nothing

-- | Pretty-print an arithmetic expression.
--
-- >>> render (Number 10)
-- "10.0"
-- >>> render (Add (Number 10) (Number 20))
-- "(10.0) + (20.0)"
render
    :: (Show a, Eq a, Floating a)
    => Expr a -> String
render (Number x)       = show x
render (Add a b)        = "(" ++ render a ++ ") + (" ++ render b ++ ")"
render (Subtract a b)   = "(" ++ render a ++ ") - (" ++ render b ++ ")"
render (Multiply a b)   = "(" ++ render a ++ ") * (" ++ render b ++ ")"
render (Divide a b)     = "(" ++ render a ++ ") / (" ++ render b ++ ")"
render (Modulus a b)    = "(" ++ render a ++ ") % (" ++ render b ++ ")"
render (Exp a b)        = "(" ++ render a ++ ") ^ (" ++ render b ++ ")"
render (Logarithm a b)
  | a == Number 10      = "log (" ++ render b ++ ")"
  | a == Number (exp 1) = "ln (" ++ render b ++ ")"
  | otherwise           = "log_(" ++ render a ++ ") (" ++ render b ++ ")"
render (Factorial e)    = "(" ++ render e ++ ")!"
render (Negate e)       = "-(" ++ render e ++ ")"
