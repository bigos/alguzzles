import Control.Monad
import Data.Char
import Data.List

testgrid = ["b---d", "-d--d", "--dd-", "--d--", "----d"]
testgrid2 = ["b---d", "-d--d", "-----", "--d--", "----d"]

dpos pos offset = (fst pos + fst offset, snd pos + snd offset)

botVector pos goal = (fst goal - fst pos, snd goal - snd pos)

cellAtPosition pos grid = row !! snd pos
  where row = grid !! fst pos

onDirt pos grid = cellAtPosition pos grid == 'd'

positions radius = join [ne, es, sw, wn]
  where
    zeroRadius = [0 .. radius]
    radiusZero = reverse zeroRadius
    negRadiusZero = [(negate radius) .. 0]
    zeroNegRadius = reverse negRadiusZero
    ne = init $ zip zeroRadius negRadiusZero
    es = init $ zip radiusZero zeroRadius
    sw = init $ zip zeroNegRadius radiusZero
    wn = init $ zip negRadiusZero zeroNegRadius

dirtPositions botPos radius grid = filter relevant dPoses
  where
    radiusPositions = positions radius
    dPoses = map (\p -> dpos botPos p) radiusPositions
    relevant d = ((elem (fst d) [0..4] && elem (snd d) [0..4]) && onDirt d grid)

direction vector
  | fst vector < 0 = "LEFT"
  | fst vector > 0 = "RIGHT"
  | snd vector < 0 = "UP"
  | snd vector > 0 = "DOWN"

getDirection :: (Int, Int) -> [String] -> String
getDirection pos grid = direction (botVector pos (head dirty))
  where
    dirtsCoord = join $ map (\r -> dirtPositions pos r grid) [1..8]
    dirty = filter (\dc -> onDirt dc grid) dirtsCoord


nextMove :: (Int, Int) -> [String] -> String
nextMove pos grid =
  if (onDirt pos grid)
  then "CLEAN"
  else getDirection pos grid

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
  print (nextMove (r, c) grid)
