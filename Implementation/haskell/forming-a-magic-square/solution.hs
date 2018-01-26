-- forming a magic square

-- looks like there might be another way to do it instead of generating magic
-- squares, try to see if adding up square values will give us the clue

import Data.List
import Data.Tree
import Control.Monad
import Debug.Trace

squareLns ar = [r1, r2, r3, c1, c2, c3, d1, d2]
  where r1 = map (\x -> ar!!x) [0,1,2]
        r2 = map (\x -> ar!!x) [3,4,5]
        r3 = map (\x -> ar!!x) [6,7,8]
        c1 = map (\x -> ar!!x) [0,3,6]
        c2 = map (\x -> ar!!x) [1,4,7]
        c3 = map (\x -> ar!!x) [2,5,8]
        d1 = map (\x -> ar!!x) [0,4,8]
        d2 = map (\x -> ar!!x) [2,4,6]

squareSums ar = [r1, r2, r3, c1, c2, c3, d1, d2]
  where r1 = sum $ map (\x -> ar!!x) [0,1,2]
        r2 = sum $ map (\x -> ar!!x) [3,4,5]
        r3 = sum $ map (\x -> ar!!x) [6,7,8]
        c1 = sum $ map (\x -> ar!!x) [0,3,6]
        c2 = sum $ map (\x -> ar!!x) [1,4,7]
        c3 = sum $ map (\x -> ar!!x) [2,5,8]
        d1 = sum $ map (\x -> ar!!x) [0,4,8]
        d2 = sum $ map (\x -> ar!!x) [2,4,6]

-- filter (\x -> length (nub (squareSums x )) ==1 ) ( permutations [1..9])
-- gives list of value permutations for magic square
msp = [[2,9,4,7,5,3,6,1,8],[2,7,6,9,5,1,4,3,8],[8,3,4,1,5,9,6,7,2],[8,1,6,3,5,7,4,9,2],
       [4,9,2,3,5,7,8,1,6],[4,3,8,9,5,1,2,7,6],[6,7,2,1,5,9,8,3,4],[6,1,8,7,5,3,2,9,4]]

      -- so i need to calculate smallest distance from example square to one of the above

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

data1 = [[4,9,2],[3,5,7],[8,1,5]]
