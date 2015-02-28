import Control.Monad
import Data.List
import Debug.Trace

process ::[[Int]] -> Int
process ar = 1 -- fixme

getData :: IO (Int, Int,[[Int]])
getData = do
  dd <- fmap (map read . words) getLine
  let n = head dd
  let m = last dd
  ar <- replicateM m (fmap (map read . words) getLine)
  return (n, m, ar)

main :: IO ()
main = do
  inputs <- getData
  let (_, _, ar) = inputs
  -- print inputs
  -- print ar
  print (process  ar)


-- graph in the example test case
--
--    5
--    |
-- 1--2--3--4
-- |  |
-- 6   7
-- |
-- 8--10
-- |
-- 9
