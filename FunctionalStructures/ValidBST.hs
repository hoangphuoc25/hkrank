import qualified Data.List
import Control.Monad

data Tree a = Nil | Node (Tree a) a (Tree a) 
  deriving Show

empty :: (Ord a) => Tree a -> Bool
empty Nil = False
empty _   = True

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil a = Node Nil a Nil
insert (Node l x r) a
  | x == a = Node l x r
  | x >  a = Node (insert l a) x r
  | otherwise = Node l x (insert r a)
  
ctree :: (Ord a) => [a] -> Tree a
ctree [] = Nil
ctree (h:t) = ctree2 (Node Nil h Nil) t
  where
    ctree2 tr [] = tr
    ctree2 tr (h:t) = ctree2 (insert tr h) t
    
preorder :: (Ord a) => Tree a -> [a]
preorder Nil = []
preorder (Node l x r) = [x] ++ preorder l ++ preorder r

isValid :: (Ord a) => [a] -> String
isValid x
    | preorder (ctree x) == x = "YES"
    | otherwise = "NO"

main :: IO ()
main = do
    firstLine <- getLine
    let noTestCase = read firstLine :: Int
    forM_ [1..noTestCase] $ \x -> do
        _ <- getLine
        array <- getLine
        let numArray = map (\a -> read a :: Int) (words array)
            result = isValid numArray
        putStrLn result

