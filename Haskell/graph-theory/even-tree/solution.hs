import Control.Monad
import Data.Graph
import Data.List
import Debug.Trace

mygr = buildG (minmaxvert ar) (allverts ar)
  where ar = [[2,1],[3,1],[4,3],[5,2],[6,1],[7,2],[8,6],[9,8],[10,8]]

-- removing edges from a graph
diflit = (edges mygr) \\ [(1,3),(3,1)]

allverts :: [[Int]] -> [(Int, Int)]
allverts ar = (map (\x -> (last x, head x)) ar) ++ (map (\x -> (head x, last x)) ar)

minmaxvert :: [[Int]] -> (Int, Int)
minmaxvert ar = (minimum (map (\x -> (minimum x) ) ar), maximum (map (\x -> (maximum x)) ar) )

process :: Int -> Int -> [[Int]] -> Int
process n m ar
  | otherwise = 1


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
  print (minmaxvert ar)
  let mygraph = buildG (minmaxvert ar) (allverts ar)
  print mygraph
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