-- picking numbers

import Data.List
import Debug.Trace

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb m (x:xs) = map (x:) (comb (m-1) xs) ++ comb m xs

sm :: [Int] -> Int -> Int -> Int -> (Int, Int) -> Int
sm [] prev sct pct maxes = ((fst maxes) + (snd maxes))
sm (x:xs) prev sct pct maxes
  | x == prev = sm xs x (succ sct) pct maxes
  | x /= prev = sm xs x 1          sct newmaxes
  where newmaxes = if ((sct + pct) > (fst maxes) + (snd maxes)) then (sct, pct) else maxes
        debugme = "calling " ++ show x ++ "---" ++ show (prev,sct,pct,maxes)

smq d = sm (sort d) 0 0 0 (0,0)

--solve :: Int -> [Int] -> Int
solve t d = 1
-- solve t d = maximum $ map (\x ->  maximum x) pairs
--   where downto = reverse [2..(pred t)]
--         multisets = map (\z -> comb z d) downto
--         max1 a  = maximum a - minimum a <= 1
--         pairs = map (\x ->
--                        map (\y ->
--                               if (max1 y) then length y else 0
--                            ) x) multisets

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

-- main = readInputData >>= (\d -> print (solve (fst d) (snd d)))
main = readInputData >>= (\d -> print (smq (snd d)))
