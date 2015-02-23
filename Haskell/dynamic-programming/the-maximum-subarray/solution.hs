import Control.Monad
import Data.List
import Debug.Trace

-- kodan [2,-1,2,3,4,-5] 0 0 (-10000)
kodan :: [Int] -> Int -> Int -> Int -> Int
kodan [] max_ending_here max_so_far largest
  | trace (show (max_ending_here, max_so_far, largest) ++ " <====== ") False = undefined
  | otherwise = if max_so_far == 0 then largest else max_so_far

kodan ar max_ending_here max_so_far largest_neg
  | trace (show (ar, max_ending_here, max_so_far, largest_neg) ++ " <++++++ ") False = undefined
  | otherwise = kodan (tail ar) max_ending_here2 max_so_far2 largest_neg2
  where x = head ar
        max_ending_here2 = (0 `max` (max_ending_here) + x)
        max_so_far2 = max_so_far `max` max_ending_here2
        largest_neg2 = if x > largest_neg then x else largest_neg

-- not finished yet, just copied kodan
-- non contiguous max will be here
kodanNc :: [Int] -> Int -> Int -> Int -> Int
kodanNc [] max_ending_here max_so_far largest
  | trace (show (max_ending_here, max_so_far, largest) ++ " <====== ") False = undefined
  | otherwise = largest
kodanNc ar max_ending_here max_so_far largest_neg
  | trace (show (ar, max_ending_here, max_so_far, largest_neg) ++ " <++++++ ") False = undefined
  | otherwise = kodan (tail ar) max_ending_here2 max_so_far2 largest_neg2
  where x = head ar
        max_ending_here2 = (0 `max` (max_ending_here) + x)
        max_so_far2 = max_so_far `max` max_ending_here2
        largest_neg2 = if x > largest_neg then x else largest_neg







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
