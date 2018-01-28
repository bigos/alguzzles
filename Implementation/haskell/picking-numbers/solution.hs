-- picking numbers

import Data.List

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb m (x:xs) = map (x:) (comb (m-1) xs) ++ comb m xs

--solve :: Int -> [Int] -> Int
solve t d = maximum $ map (\x -> maximum x) pairs
  where downto = reverse [2..(pred t)]
        multisets = map (\z -> comb z d) downto
        abz z = abs (z!!0 - z!!1)
        pairs = map (\x ->
                       map (\y ->
                         (if (all (\e -> e<2) (map (\z -> abz z) (comb 2 y))) then length y else 0)
                           ) x) multisets

readLineInts :: IO [Int]
readLineInts = getLine >>=
  (\l -> return (map (\x -> (read x) :: Int) (words l)))

readInputData ::  IO (Int, [Int])
-- readInputData = do
--   d1 <- readLineInts
--   d2 <- readLineInts
--   return (head d1, d2)
readInputData = readLineInts >>=
  (\d1 -> readLineInts >>=
    (\d2 -> return ((head d1), d2)))

main = readInputData >>= (\d -> print (solve (fst d) (snd d)))
