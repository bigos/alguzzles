import Control.Monad
import Data.List

getData :: IO (Int, [Int])
getData = do
  n <- readLn :: IO Int
  ar <- fmap (map str2Int . words) getLine
  return (n, ar)

str2Int :: String -> Int
str2Int = read :: String -> Int

-- slices :: Eq a => [a] -> [[a]]
-- slices [] = []
-- slices xs = tail $ inits xs ++ slices (tail xs)

maxcont ar = maximum (scanl (\acc x -> acc+x) 0 ar)
-- maxcont ar =  maximum ( map (\x -> sum x) (slices ar))
maxnoncont ar = (foldl (\acc x -> acc+x) 0 (filter (\y -> y>=0) ar))

-- maxnoncont ar = if res == 0 then maxcont ar else res
--   where res = maximum ( map (\x -> sum (filter (\y -> y >= 0) x)) (slices ar))

maxSubarray :: (Num a, Ord a) => [a] -> a
maxSubarray [x] = x
maxSubarray (x:xs) = snd $ foldl msReducer (x, x) xs

msReducer :: (Num a, Ord a) => (a, a) -> a -> (a, a)
msReducer (maxEndingHere, maxSoFar) x = (meh, maxSoFar `max` meh)
    where meh = x `max` (maxEndingHere + x)

maxsubseq = snd . foldl f ((0,[]),(0,[])) where
	f ((h1,h2),sofar) x = (a,b) where
		a = max (0,[]) (h1 + x, h2 ++ [x])
		b = max sofar a

max_sublist l = reverse $ fst $ foldl find_max ([],(0,[],0)) l where
  find_max (max, (sum, r, rsum)) x = (max', (sum', r', rsum'))
    where
    (r', rsum')  = if (rsum > 0)
                   then (x:r, rsum+x)
                   else ([x], x)
    (max', sum') = if (rsum' > sum)
                   then (r', rsum')
                   else (max, sum)

showmaxes :: [Int] -> String
showmaxes ar = show (maxcont ar) ++ " " ++show (maxnoncont ar)

main :: IO ()
main = do
  t <- readLn :: IO Int
  -- print t
  inputs <- replicateM t getData :: IO [(Int, [Int])]
  mapM_ (\x -> putStrLn (showmaxes (snd x))) inputs
