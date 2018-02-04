{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Control.Monad

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

inOrderTraverse :: Tree a -> [a]
inOrderTraverse Nil = []
inOrderTraverse (Node a x y) = inOrderTraverse x ++ [a] ++ inOrderTraverse y
swap :: Tree a -> Tree a 
swap Nil = Nil
swap (Node a x y) = Node a y x

swapOp :: Int -> Int -> Tree a -> Tree a
swapOp _ _ Nil = Nil
swapOp h k (Node a x y) = if h == k then Node a y x else Node a (swapOp (h+1) k x) (swapOp (h+1) k y)

swapOpMain :: Int -> Tree a -> Tree a
swapOpMain h tree = foldr (\cur acc -> swapOp 1 cur acc) tree [i | i <- [1..height tree], mod i h == 0]

printList :: Show a => [a] -> String
printList l = intercalate " " (map show l)

height :: Tree a -> Int
height Nil      = 0
height (Node a x y) = 1 + max (height x) (height y)

main1 :: IO ()
main1 = do
  let a = Node 1 (Node 2 Nil (Node 4 Nil Nil)) (Node 3 Nil (Node 5 Nil Nil))
  putStrLn $ printList $ inOrderTraverse $ swap $ a
  putStrLn $ printList $ inOrderTraverse $ swapOpMain 2 a

  let b = Node 1 (Node 2 (Node 4 (Node 6 Nil (Node 9 Nil Nil)) Nil) Nil)
                 (Node 3 (Node 5 (Node 7 Nil Nil) (Node 8 (Node 10 Nil Nil) (Node 11 Nil Nil))) Nil)
  putStrLn $ printList $ inOrderTraverse $ swapOpMain 2 b

makeTree :: [(Int, Int)] -> Int -> Tree Int
makeTree t idx =
  case t!!idx of
    (-1, -1) -> Node idx Nil Nil
    (-1,  _) -> Node idx Nil (makeTree t (snd (t!!idx)))
    ( _, -1) -> Node idx (makeTree t (fst (t!!idx))) Nil
    ( _,  _) -> Node idx (makeTree t (fst (t!!idx))) (makeTree t (snd (t!!idx)))

main :: IO ()
main = do
  noNodes <- read <$> getLine
  allLines <- mapM (\_ -> words<$>getLine) [1..noNodes]
  let lefts = [0] ++ map ((read :: String -> Int).head) allLines
      rights = [0] ++ map ((read :: String -> Int).head.tail) allLines
      zipped = zipWith (,) lefts rights
      tree = makeTree zipped 1
  noQuery <- read <$> getLine
  allQueries <- mapM (\_ -> (read::String->Int)<$>getLine) [1..noQuery]
  let finalTree = tail (scanl (\b a -> swapOpMain a b) tree allQueries)
  putStrLn $ intercalate "\n" $ map (printList.inOrderTraverse) finalTree 
