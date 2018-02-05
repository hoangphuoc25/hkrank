import Data.Array
import Debug.Trace

printBoard :: Array (Int, Int) Int -> String
printBoard a = init (unlines [unwords [show (a ! (x, y)) | y <- [1..(snd.snd.bounds) a]] | x <- [1..(fst.snd.bounds) a]])

rotateString :: Int -> [a] -> [a]
rotateString _ [] = []
rotateString r lst = drop x lst ++ take x lst
  where x = mod r (length lst)

rotateBoard :: Int -> Array (Int,Int) Int -> Array (Int,Int) Int
rotateBoard r arr = 
  let hs = (fst.fst.bounds) arr
      ws = (snd.fst.bounds) arr
      h = (fst.snd.bounds) arr
      w = (snd.snd.bounds) arr
  in
    array ((hs, ws), (h, w)) (zipWith (\a b -> (a, arr!b)) (spiralIndices (1,1) (h,w)) (rotateIndices (1,1) (h,w) r))

rotateIndices :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
rotateIndices (hs, ws) (h,w) r =
  let perimIndices = [(hs, i) | i <- [ws..w]] ++ [(x, w) | x <- [hs+1..h]] ++ [(h, i) | i <- [w-1,w-2..ws]] ++ [(x,ws) | x <- [h-1,h-2..hs+1]] 
      newPerimIndices = rotateString r perimIndices
  in
    if hs >= h ||  ws >= w then []
      else newPerimIndices ++ rotateIndices (hs+1,ws+1) (h-1,w-1) r

spiralIndices :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
spiralIndices (hs, ws) (h,w) =
  let perimIndices = [(hs, i) | i <- [ws..w]] ++ [(x, w) | x <- [hs+1..h]] ++ [(h, i) | i <- [w-1,w-2..ws]] ++ [(x,ws) | x <- [h-1,h-2..hs+1]] 
  in
    if hs >= h || ws >= w then []
      else perimIndices ++ spiralIndices (hs+1,ws+1) (h-1,w-1)

main :: IO ()
main = do
  firstLine <- (map (read :: String -> Int).words) <$> getLine
  let h = head firstLine
      w = (head.tail) firstLine
      r = (head.tail.tail) firstLine
  board <- mapM (\_ -> (map (read :: String -> Int).words) <$> getLine) [1..h]
  let board' = array ((1,1), (h, w)) [((i,j), (board!!(i-1))!!(j-1)) | i <- [1..h], j <- [1..w]]
  putStr $ printBoard $ rotateBoard r board'