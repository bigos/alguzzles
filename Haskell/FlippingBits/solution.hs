import Data.List
import Control.Monad
import Data.Bits
import Data.Word

-- run it in terminal
-- $ cat ./input1.txt | runhaskell solution.hs

main :: IO ()
main = do
  t0 <- readLn :: IO Int
  str <- replicateM t0 getLine
  let nums = map read str :: [Word32]
  print nums
  print (complement (head nums) )
