import Control.Monad

getData :: IO (Int, [Int])
getData = do
  n <- readLn :: IO Int
  ar <- fmap (map str2Int . words) getLine
  return (n, ar)

str2Int :: String -> Int
str2Int = read :: String -> Int

main :: IO ()
main = do
  t <- readLn :: IO Int
  print t
  inputs <- replicateM t getData :: IO [(Int, [Int])]
  mapM_ print inputs
