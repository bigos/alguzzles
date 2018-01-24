-- forming a magic square

-- looks like there might be another way to do it instead of generating magic
-- squares, try to see if adding up square values will give us the clue

import Control.Monad
import Debug.Trace

-- finding bounds depending on direction
edge :: (Int, Int) -> (Int, Int) -> Bool
edge c d
  | d == (0, 1) = snd c > 2
  | d == (1, 0) = fst c > 2
  | d == (1, (-1)) = fst c > 2
  | d == (1, 1) = snd c > 2

travar ::  (Int, Int) -> (Int, Int) -> [[Int]] -> [Int] -> [Int]
travar c d ar ac = if (edge c d) then reverse ac else travar newc d ar (cv:ac)
  where newc = (fst c + fst d, snd c + snd d)
        cv = ar !! fst c !! snd c

rcd :: [[Int]] -> [[Int]]
rcd ar = [                      -- all value ranges of magic square
  travar (0,0) (1, 0) ar [],     -- columns
  travar (0,1) (1, 0) ar [],
  travar (0,2) (1, 0) ar [],
  travar (0,0) (0, 1) ar [],     -- rows
  travar (1,0) (0, 1) ar [],
  travar (2,0) (0, 1) ar [],
  travar (0,0) (1,  1) ar [],     -- diagonals
  travar (0,2) (1, -1) ar []]

rcdSums ar = map sum (rcd ar)

-- readLineInts = do
--   line <- getLine
--   return (map (\x -> (read x) ::Int) (words line))
readLineInts :: IO [Int]
readLineInts = getLine >>=
  (\l -> return (map (\x -> (read x) :: Int) (words l)))

-- main :: IO ()
-- main = do
--   d <- replicateM r readLineInts
--   print d
main = (replicateM 3 readLineInts) >>= print
