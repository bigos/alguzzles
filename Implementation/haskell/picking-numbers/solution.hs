-- picking numbers

import Data.List
import Debug.Trace

sm :: [Int] -> Int -> Int -> Int -> Int -> Int
sm [] prev sct pct maxes = max maxes (sct+pct)
sm (x:xs) prev sct pct maxes
  | x == prev = sm xs x (succ sct) pct maxes
  | x /= prev = sm xs x 1          sct newmaxes
  where newmaxes = if ((sct + pct) > maxes) then (sct + pct) else maxes

smq d = sm (sort d) 0 0 0 0

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

main = readInputData >>= (\d -> print (smq (snd d)))
