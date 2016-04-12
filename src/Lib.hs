-- | Example of a library file. It is also used for testing the test suites.
module Lib
  ( parse
  , evaluate
  , Expr
  ) where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Expr (Operator(Prefix, InfixL, Postfix), makeExprParser)
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L
import Data.Fixed (mod')

data Expr a =
      Number a
    | Add        (Expr a) (Expr a)
    | Subtract   (Expr a) (Expr a)
    | Multiply   (Expr a) (Expr a)
    | Divide     (Expr a) (Expr a)
    | Modulus    (Expr a) (Expr a)
    | Exp        (Expr a) (Expr a)
    | Logarithm  (Expr a) (Expr a)
    | Factorial  (Expr a)
    | Negate     (Expr a)
    deriving (Show, Eq)

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

word :: String -> Parser ()
word w = string w *> notFollowedBy alphaNumChar *> sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operators :: [[Operator Parser (Expr Double)]]
operators =
  [ [
      Prefix  (symbol "-" *> pure Negate)
    , Postfix (symbol "!" *> pure Factorial) ]
  , [ InfixL  (symbol "^" *> pure Exp)
    , Prefix  (symbol "e" *> symbol "^" *> pure (Exp (Number $ exp 1))) ]
  , [ InfixL  (symbol "*" *> pure Multiply)
    , InfixL  (symbol "/" *> pure Divide)
    , InfixL  (symbol "%" *> pure Modulus) ]
  , [ InfixL  (symbol "+" *> pure Add)
    , InfixL  (symbol "-" *> pure Subtract) ]
  , [ Prefix  (word "ln" *> pure (Logarithm (Number $ exp 1)))
    , Prefix  (word "log" *> pure (Logarithm (Number 10))) ]
  ]

signedFloat :: Parser Double
signedFloat = L.signed sc $ either fromIntegral id <$> L.lexeme sc L.number

term :: Parser (Expr Double)
term = parens expr <|> Number <$> signedFloat

expr :: Parser (Expr Double)
expr = makeExprParser term operators

-- | Parse an algebraic expression.
--
-- >>> parse "x"
-- Left 1:1:
-- unexpected 'x'
-- expecting '(', '+', '-', or number
-- >>> parse "1"
-- Right (Number 1.0)
parse :: String -> Either ParseError (Expr Double)
parse = runParser expr ""


evaluate :: RealFloat a => Expr a -> Maybe a
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
    if y' == 0 then Nothing else Just (x' / y')
evaluate (Factorial x) = do
    x' <- evaluate x
    -- If x' approximates an integer
    if x' == fromInteger (round x')
        then Just $ fromInteger $ product [1..(round x' :: Integer)]
        else Nothing
