-- picking numbers

import Data.List

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb m (x:xs) = map (x:) (comb (m-1) xs) ++ comb m xs

--solve :: Int -> [Int] -> Int
solve t d = 1

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
