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

-- *Main> let reme = remedges mygr [[1,3],[1,6]]
-- *Main> components reme
-- *Main> map (\x -> flatten x) (components reme) -- yess!!!

-- this one finds correct results
gettingThere = ( (map (\x -> (length $ join $ forests mygr x,x, numberOfOddForests mygr x)) (vertComb (take 5 arr))))



-- third result is the puzzle solution
-- many results are not forests, but single nodes
zeroOddforests = filter (\y -> fst y == 0) ress

vertlen :: [Vertex] -> Int
vertlen vv = length (vv)

finalResult =  maximum $ map (\x ->  length x)  ress2

ress2 = map (\y -> fst (snd y)) $ filter (\x -> (snd (snd x)) == vertlen( vertices mygr)) ress

ress ::[ (Int,( [[Int]], Int))]
ress = filter (\y -> fst y == 0) $ map (\x -> ((numberOfOddForests mygr x) ,( x, ( length (join $ forests mygr x)))) ) (vertComb ( arr))

vertComb :: [[Int]] -> [[[Int]]]
vertComb ar = tail $ init $ subsequences (ar)

sortListOfLists arl = groupBy ((==) `on` length) $ sortBy (compare `on` length) arl

numberOfOddForests :: Graph -> [[Int]] -> Int
numberOfOddForests gr remd = length $ filter (\x -> odd (length x)) (forests gr remd)

forests gr remd = map (\x -> flatten x) (components reme)
  where reme = remedges gr remd

-- removing edges from a graph
diflit = (edges mygr) \\ (allverts [[1,3],[6,1]])

remedges :: Graph -> [[Int]] -> Graph
remedges gr reme = buildG range newedges
  where newedges = (edges gr) \\ (allverts reme)
        eds = map (\x ->  fst x) newedges ++ map (\x -> snd x) newedges
        range = (minimum eds, maximum eds)


wow nd = reachable grWithRemovedEdges nd -- nd is a node number eg. 6
  where gr = mygr
        grWithRemovedEdges = remedges gr [[1,3],[1,6]]
        res1 = reachable grWithRemovedEdges 1
        res2 = reachable grWithRemovedEdges 6 -- finds connections in decomposed tree

-- last thing to do is removing edge combinations


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
