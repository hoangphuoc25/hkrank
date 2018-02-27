{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Parsec.Token as Tok
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import Data.List ((\\), sortOn, isPrefixOf, filter)
import Control.Monad

langDef :: Tok.LanguageDef ()
langDef = emptyDef {
  Tok.opStart = oneOf "+-/*",
  Tok.opLetter = oneOf "+-/*"
}

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

integer    = Tok.integer    lexer
parens     = Tok.parens lexer

singleTerm = do
  x <- integer
  return $ Lit x

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
          | Lit Integer
          deriving (Show)

p = 1000000007
eval :: Expr -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = (eval a) * modInv (finalEval b) p
eval (Neg a) = eval a * (-1)

finalEval :: Expr -> Integer
finalEval x = mod (eval x) p

---
expr            = buildExpressionParser aOperators singleTermExpr
singleTermExpr  = parens expr <|> singleTerm

multipliedParen = do { i <- singleTerm ; e <- parens expr ; return (Mul i e); }
neg = do {i <- char '-'; x <- integer; return (Neg (Lit x)); }

aOperators = [[Prefix (char '-' >> return (Neg))],
              [Infix (char '*' >> return (Mul)) AssocRight,
              Infix (char '/' >> return (Div)) AssocRight  ],
              [Infix (char '+' >> return (Add)) AssocRight,
              Infix (char '-' >> return (Sub)) AssocRight] ]

-- reversed euclidean
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)

modInv a m = let (i, _, g) = gcdExt a m
             in (mkPos i)
  where mkPos x = if x < 0 then x + m else x

main :: IO ()
main = do
  input <- getLine
  let noSpace = filter (/=' ') input
  case (parse expr "" noSpace) of
    Left error -> print "error"
    Right parsed -> print $ finalEval parsed
