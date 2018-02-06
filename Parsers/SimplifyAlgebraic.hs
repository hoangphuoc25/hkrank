{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Parsec.Token as Tok
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import Data.List ((\\), sortOn, isPrefixOf, filter, intercalate)
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

singleTerm =
  do x <- option 1 integer
     y <- option 0 (do {char 'x'; option 1 (do { char '^'; y <- integer; return y; }); })
     return $ Lit [(x,y)]

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
          | Parens Expr
          | Lit [(Integer,Integer)]
          deriving (Show)

expr    = buildExpressionParser aOperators singleTermExpr
singleTermExpr = parens expr <|> Text.ParserCombinators.Parsec.try multipliedParen  <|> singleTerm

multipliedParen = do { i <- singleTerm ; e <- parens expr ; return (Mul i e); }

aOperators = [[Prefix ((Tok.reservedOp lexer) "-" >> return (Neg))],
              [Infix ((Tok.reservedOp lexer) "*" >> return (Mul)) AssocLeft,
              Infix ((Tok.reservedOp lexer) "/" >> return (Div)) AssocLeft],
              [Infix ((Tok.reservedOp lexer) "+" >> return (Add)) AssocLeft,
              Infix ((Tok.reservedOp lexer) "-" >> return (Sub)) AssocLeft]]

eval :: Expr -> [Integer]
eval (Lit a) = result
  where
    y = a ++ [(0,i) | i <- ([0..maximum (map snd a)] \\ (map snd a)) ]
    result = map fst (sortOn snd y)
eval (Add a b) = result 
  where
    x = eval a
    y = eval b
    x' = x ++ [0 | i <- [1..max (length x) (length y) - length x]]
    y' = y ++ [0 | i <- [1..max (length x) (length y) - length y]]
    result = zipWith (+) x' y'
eval (Sub a b) = result
  where
    x = eval a
    y = eval b
    x' = x ++ [0 | i <- [1..max (length x) (length y) - length x]]
    y' = y ++ [0 | i <- [1..max (length x) (length y) - length y]]
    result = zipWith (-) x' y'
eval (Mul a b) = result
  where
    m = eval a
    n = eval b
    o = map (\(i,j) -> [ 0 |x<-[1..j-1]] ++ map (*i) n) (zip m [1..length m])
    p = map (\i -> i ++ [ 0 | x <- [1..length m + length n - 1 - length i]]) o
    result = foldr1 (zipWith (+)) p
eval (Div a b) = result
  where
    x = eval a
    y = head (eval b)
    result = map (\i -> div i y) x
eval (Neg a) = map (*(-1)) (eval a)

printSingleTerm :: Integer -> Integer -> [String]
printSingleTerm 0 power = []
printSingleTerm coef power = [sign coef, f (abs coef) power]
  where
    sign :: Integer -> String
    sign x | x >= 0 = "+"
           | otherwise = "-"
    f :: Integer -> Integer -> String
    f 0 _ = ""
    f 1 1 = "x"
    f x 0 = show x
    f x 1 = show x ++ "x"
    f 1 y = "x^" ++ show y
    f x y = show x ++ "x^" ++ show y

printExpr :: [Integer] -> String
printExpr exp = result
  where
    expWithIdx = zip (reverse exp) (map toInteger [length exp-1, length exp-2..0])
    a = map (\(x,y) -> printSingleTerm x y) (tail expWithIdx)
    b = " " ++ intercalate " " (concat a)
    c = concat (printSingleTerm (last exp) (toInteger (length exp-1)))
    d = if isPrefixOf "+" c then tail c else c
    result = d ++ b


showSign :: Integer -> String
showSign x = if x >= 0 then "+" ++ show x else "-" ++ show (abs x)

main :: IO ()
main = do
  noQueries <- read <$> getLine
  queries <- mapM (\_ -> getLine) [1..noQueries]
  let noSpace = map (filter (/=' ')) queries
  forM_ [0..noQueries-1] $ \i -> do
    case (parse expr "" (noSpace!!i)) of
      Left error -> print "error"
      Right parsed -> putStrLn $ printExpr $ eval parsed