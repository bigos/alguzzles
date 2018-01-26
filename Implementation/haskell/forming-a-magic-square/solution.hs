-- forming a magic square

import Data.List
import Data.Tree
import Control.Monad
import Debug.Trace

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

dist a1 a2 =  sum $ map (\x -> abs (fst x - snd x) ) (zip a1 a2)

mindist a = minimum (map (\x -> dist a x) msp)

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
main = do
  a <- replicateM 3 readLineInts
  print $ mindist (join a)
