import Control.Monad
import Data.List

getData :: IO (Int, [Int])
getData = do
  n <- readLn :: IO Int
  ar <- fmap (map str2Int . words) getLine
  return (n, ar)

str2Int :: String -> Int
str2Int = read :: String -> Int

slices :: Eq a => [a] -> [[a]]
slices [] = []
slices xs = tail $ inits xs ++ slices (tail xs)

maxcont ar =  maximum ( map (\x -> sum x) (slices ar))
maxnoncont ar = maximum ( map (\x -> sum (filter (\y -> y >= 0) x)) (slices ar))

showmaxes :: [Int] -> String
showmaxes ar = show (maxcont ar) ++ " " ++show (maxnoncont ar)

main :: IO ()
main = do
  t <- readLn :: IO Int
  -- print t
  inputs <- replicateM t getData :: IO [(Int, [Int])]
  mapM_ (\x -> putStrLn (showmaxes (snd x))) inputs
