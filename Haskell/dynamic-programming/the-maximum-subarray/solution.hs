import Control.Monad
import Data.List
import Debug.Trace


kodan [] max_ending_here max_so_far
  | trace (show max_so_far ++ " <====== ") False = undefined
  | otherwise = max_so_far

kodan ar max_ending_here max_so_far
  | trace (show (ar, max_ending_here, max_so_far) ++ " <++++++ ") False = undefined
  | otherwise = kodan (tail ar) max_ending_here max_so_far

getData :: IO (Int, [Int])
getData = do
  n <- readLn :: IO Int
  ar <- fmap (map str2Int . words) getLine
  return (n, ar)

str2Int :: String -> Int
str2Int = read :: String -> Int

maxSubarray :: (Num a, Ord a) => [a] -> a
maxSubarray [x] = x
maxSubarray (x:xs) = snd $ foldl msReducer (x, x) xs

msReducer :: (Num a, Ord a) => (a, a) -> a -> (a, a)
msReducer (maxEndingHere, maxSoFar) x = (meh, maxSoFar `max` meh)
    where meh = x `max` (maxEndingHere + x)

showmaxes :: [Int] -> String
showmaxes ar = show (maxSubarray ar) ++ " " ++show ( maxSubarray $ sort  ar)

main :: IO ()
main = do
  t <- readLn :: IO Int
  -- print t
  inputs <- replicateM t getData :: IO [(Int, [Int])]
  mapM_ (\x -> putStrLn (showmaxes (snd x))) inputs
