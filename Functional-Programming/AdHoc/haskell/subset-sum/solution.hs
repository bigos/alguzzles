-- solution.hs - running in terminal $ cat ./input0.txt | runhaskell solution.hs
-- stucck :-(

import Control.Monad

solve :: Int -> [Int] -> Int -> (Int,[Int],Int)
solve n a s = (n, a, s)

main :: IO ()
main = do
  n <- getLine
  a <- getLine
  tc <- getLine
  eses <- replicateM (read tc) getLine
  -- print  (n,a, tc, eses)
  map (\x -> solve n a x) eses
