import Control.Monad

princessColumn n "" = -1
princessColumn n ( 'p' : _) = n
princessColumn n str = princessColumn  (succ n) (drop 1 str)
princessPosition n grid = head $ filter (\(a,b) -> b >= 0) coords
  where
  columns = (map (\x -> princessColumn 0 x) grid)
  coords  = zip [0..n] columns

positionDifference bp pp = ((fst pp) - (fst bp), (snd pp) - (snd bp))

direction pv
  | yd < 0 = "LEFT"
  | yd > 0 = "RIGHT"
  | xd < 0 = "UP"
  | xd > 0 = "DOWN"
  where
    xd = fst pv
    yd = snd pv

nextMove n r c grid = direction (positionDifference (r,c) (princessPosition n grid))

main :: IO ()
main = do
  line1 <- getLine
  line2 <- getLine
  let n = read line1 :: Int
  let botPosition = map read (words line2) :: [Int]
  let r = botPosition !! 0
  let c = botPosition !! 1
  grid <- replicateM n getLine
  -- print n
  -- print botPosition
  -- print r
  -- print c
  -- print grid
  putStrLn (nextMove n r c grid)
