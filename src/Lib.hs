-- | Example of a library file. It is also used for testing the test suites.
module Lib
  (
    -- * Exported functions
    inc
  , parse
  , evaluate
  , Expr()
  ) where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec (runParser, ParseError, (<|>), spaceChar, between)
import Text.Megaparsec.Expr (Operator(Prefix, InfixL), makeExprParser)
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

data Expr a =
      Number a
    | Hole
    | Add (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Div (Expr a) (Expr a)
    | Mod (Expr a) (Expr a)
    | Negate (Expr a)
    deriving (Show, Eq)

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operators :: Num a => [[Operator Parser (Expr a)]]
operators =
  [ [ Prefix (symbol "-" *> pure Negate) ]
  , [ InfixL (symbol "*" *> pure Mul)
    , InfixL (symbol "/" *> pure Div) ]
  , [ InfixL (symbol "+" *> pure Add)
    , InfixL (symbol "-" *> pure Sub)
    , InfixL (symbol "%" *> pure Mod) ]
  ]

signedFloat :: Parser Double
signedFloat = L.signed sc $ either fromIntegral id <$> L.lexeme sc L.number

term :: Parser (Expr Double)
term = parens expr <|> Number <$> signedFloat

expr :: Parser (Expr Double)
expr = makeExprParser term operators

-- TODO: Use runParser(T)'
parse :: String -> Either ParseError (Expr Double)
parse = runParser expr ""


evaluate :: Num a => Expr a -> a
evaluate = undefined


-- | Increment one 'Num' value.
--
--  >>> let answer = 42 :: Int
--  >>> let prev = answer - 1
--  >>> inc prev
--  42
--  >>> succ . Prelude.last . Prelude.take prev . iterate inc $ 1
--  42
--
--  Properties:
--
--  prop> succ x == inc x
--  prop> inc (negate x) == negate (pred x)
--
inc :: Num a => a -- ^ value to increment
             -> a -- ^ result
inc x = x + 1
