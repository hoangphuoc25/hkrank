
memoized_f :: Int -> Integer
memoized_f = (map f [0..] !!)
    where
        f :: Int -> Integer
        f 0 = 1
        f 1 = 1
        f x = sum (map (\j -> memoized_f (j-1) * memoized_f (x-j)) [1..x])


main :: IO ()
main = do
    noQuery <- read <$> getLine
    input <- mapM (\_ -> (read :: String->Int) <$> getLine) [1..noQuery]
    let results = map (\i -> mod (memoized_f i) 100000007)  input
    mapM_ (putStrLn.show) results
