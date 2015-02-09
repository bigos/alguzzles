import Data.List
import Control.Monad

-- run it in terminal
-- $ cat ./input1.txt | runhaskell solution.hs

main :: IO ()
main = do
  t0 <- readLn :: IO Int
  str <- replicateM t0 getLine
  let nums = map read str :: [Int]
  print nums
