import Control.Monad
import System.IO

-- run it in terminal
-- $ cat ./input1.txt | runhaskell solution.hs

main :: IO ()
main = do
  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = map (sum. map read. words) str
  mapM_ print ans
