import Control.Monad

-- run it in terminal
-- $ cat ./input1.txt | runhaskell solution.hs
lg :: Int -> Int -> Int -> Int
lg s e a =
  if s < e
  then
  if even s
  then
    lg (s+1) e (a+1)
  else
    lg (s+1) e (a*2)
  else
    a

loopGrowth :: Int -> Int
loopGrowth x = lg 0 x 0

calcGrowth :: Int -> Int
calcGrowth y =
  last (take (y+1) (tail (join [ [2 ^ x - 2 ,2 ^ x - 1] | x <- [1..50] ])))

main :: IO ()
main = do
  t <- readLn :: IO Int
  strs <- replicateM t getLine
  let cycles = map read strs :: [Int]
  -- print t
  -- print cycles
  -- let answers = map calcGrowth cycles
  let answers = map loopGrowth cycles
  forM_ answers print
