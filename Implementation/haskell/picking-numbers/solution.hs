-- picking numbers

import Data.List
import Debug.Trace

sm :: [Int] -> Int -> Int -> Int -> Int -> Int -> Int
sm [] prev pv sct pct maxes = (newmaxes (-9) prev pv sct pct maxes)
sm (x:xs) prev pv sct pct maxes
  | x == prev = sm xs x pv   (succ sct) pct maxes
  | x /= prev = sm xs x prev 1          sct (newmaxes x prev pv sct pct maxes)

newmaxes x prev pv sct pct maxes =
  if (pv == pred prev)
  then
    (if ((sct + pct) > maxes)
     then (sct + pct)
     else maxes)
  else
    (if ((sct ) > maxes)
     then (sct)
     else maxes)

smq d = sm (sort d) 0 0 0 0 0

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
