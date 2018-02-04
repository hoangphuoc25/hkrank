import Data.List

main :: IO ()
main = do
    _ <- getLine
    inputs <- map (read :: String -> Int). words <$> getLine
    let result = foldl1 lcm inputs
    print result
