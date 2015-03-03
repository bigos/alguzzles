import Control.Monad
import Data.Graph
import Data.List
import Debug.Trace

-- mygr = buildG (1,6) [(1,2),(1,3),(1,6),(2,4),(5,6)]




-- finish me
-- nodeGroup :: Int -> [Int] -> [[Int]] -> [Int]
-- nodeGroup node found ar
--   | elem node found = nub found
--   | otherwise = nodeGroup node found ar
--   where cs = connections node ar

-- connections node ar = nub $ filter (\x -> x/= node ) (join (vertices node ar))

vertices node ar = filter (\x -> (head x == node || (head $ tail x) == node) ) ar

process :: Int -> Int -> [[Int]] -> Int
process n m ar = n + 1

getData :: IO (Int, Int,[[Int]])
getData = do
  dd <- fmap (map read . words) getLine
  let n = head dd :: Int
  let m = last dd :: Int
  ar <- replicateM m (fmap (map read . words) getLine)
  return (n, m, ar)

main :: IO ()
main = do
  inputs <- getData
  let (n, m, ar) = inputs
  print n
  print m
  print ar
  print (process n m ar)


-- graph in the example test case
--
--
-- 4
-- |
-- 3  5
-- +  |
-- 1--2
-- +  |
-- 6  7
-- |
-- 8--10
-- |
-- 9
