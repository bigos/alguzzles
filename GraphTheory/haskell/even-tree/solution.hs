import Control.Monad
import Data.Graph
import Data.List
import Data.Tree
import Data.Function (on)
import Debug.Trace

arr :: [[Int]]
arr =  [[2,1],[3,1],[4,3],[5,2],[6,1],[7,2],[8,6],[9,8],[10,8]]

mygr = buildG (minmaxvert ar) (allverts ar)
  where ar = [[2,1],[3,1],[4,3],[5,2],[6,1],[7,2],[8,6],[9,8],[10,8]]

-- -------------------------------------

finalResult gr ar = maximum $ map (\x -> length x) (ress2 gr ar)

ress2 gr ar = map (\y -> fst (snd y)) $ filter (\x -> (snd (snd x)) == length (vertices gr)) (ress gr ar)

ress :: Graph -> [[Int]] ->[ (Int,( [[Int]], Int))]
ress gr ar = filter (\y -> fst y == 0) $ map (\x -> thedata gr x) (vertComb ar)

thedata gr x = ( (numberOfOddForests gr x) ,( x, (length (join $ forests gr x))) )

-- check subsequences
vertComb :: [[Int]] -> [[[Int]]]
vertComb ar = tail $ init $ subsequences (ar)

numberOfOddForests :: Graph -> [[Int]] -> Int
numberOfOddForests gr remd = length $ filter (\x -> odd (length x)) (forests gr remd)

-- check components
forests gr remd = map (\x -> flatten x) (components (remedges gr remd))

-- removing edges from a graph
diflit = (edges mygr) \\ (allverts [[1,3],[6,1]])

remedges :: Graph -> [[Int]] -> Graph
remedges gr reme = buildG range newedges
  where newedges = (edges gr) \\ (allverts reme)
        eds = map (\x ->  fst x) newedges ++ map (\x -> snd x) newedges
        range = (minimum eds, maximum eds)

allverts :: [[Int]] -> [(Int, Int)]
allverts ar = (map (\x -> (last x, head x)) ar) ++ (map (\x -> (head x, last x)) ar)

minmaxvert :: [[Int]] -> (Int, Int)
minmaxvert ar = (minimum (map (\x -> (minimum x) ) ar), maximum (map (\x -> (maximum x)) ar) )

process :: Int -> Int -> [[Int]] -> Graph -> Int
process _ _ ar gr = finalResult gr ar


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
  -- print n
  -- print m
  -- print ar
  -- print (minmaxvert ar)
  let mygraph = buildG (minmaxvert ar) (allverts ar)
  -- print mygraph
  print (process n m ar mygraph)


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
