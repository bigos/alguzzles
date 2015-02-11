import Control.Monad

-- run it in terminal
-- $ cat ./input1.txt | runhaskell solution.hs

calcGrowth y =
  last (take (y+1) (tail (join [ [2 ^ x - 2 ,2 ^ x - 1] | x <- [1..50] ])))

main :: IO ()
main = do
  t <- readLn :: IO Int
  strs <- replicateM t getLine
  let cycles = map read strs :: [Int]
  -- print t
  -- print cycles
  let answers = map calcGrowth cycles
  forM_ answers print
