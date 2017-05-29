import Control.Monad
import Data.Char
import Data.List

-- - clean cell
-- d dirty cell
-- d bot on dirty cell
-- b bot on clean cell
-- task move bot to clean all the dirty cells

testgrid = ["b---d", "-d--d", "--dd-", "--d--", "----d"]
testgrid2 = ["b---d", "-d--d", "-----", "--d--", "----d"]

rowCoordinates :: Int -> [String] -> [(Int, Int)]
rowCoordinates r grid = filter (\x -> fst x >= 0) $ zip columns [0..4]
  where
    columns = map (\x -> if x=='d' then r else -1) (grid !! r)

dirtCoordinates grid = map (\x -> rowCoordinates x grid) [0..4]

onDirt r c grid = any (\x -> x == True) results
  where
    rowDirts = (dirtCoordinates grid) !! r
    results = map (\x -> x == (r, c)) rowDirts

findDirt :: Int -> Int -> [String] -> (Int, Int)
findDirt r c grid = (1,1)
  where
    dirts = dirtCoordinates grid

getDirection :: Int -> Int -> (Int, Int) -> String
getDirection r c dirt
  | yd > 0 = "LEFT"
  | yd < 0 = "RIGHT"
  | xd > 0 = "UP"
  | xd < 0 = "DOWN"
  | otherwise = "ERROR"
  where
    xd = r - fst dirt
    yd = c - snd dirt

nextMove r c grid =
  if (onDirt r c grid)
  then "CLEAN"
  else getDirection r c (findDirt r c grid)

main :: IO ()
main = do
  line1 <- getLine
  let n = 5
  let botPosition = map read (words line1) :: [Int]
  let r = botPosition !! 0
  let c = botPosition !! 1
  grid <- replicateM n getLine
  -- print n
  -- print botPosition
  -- print r
  -- print c
  -- print grid
  print (nextMove r c grid)
