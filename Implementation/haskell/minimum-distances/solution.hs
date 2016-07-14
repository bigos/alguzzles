import Control.Applicative
import Control.Monad
import System.IO
import Debug.Trace
import Data.Array

r2 res result
  | result == (-1) = res
  | res < result = res
  | otherwise = result

setResult r rv c cv result
  | cv == rv && res > 0 = r2 res result
  | otherwise = result
  where res = c - r

-- stuck with loop within loop
-- can map over map do it?
l1 sa r rv result
  | r >= sa = result
  | otherwise = result

solve sa a = l1 sa 0 (a!!0) (-1)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    print $ solve n a
