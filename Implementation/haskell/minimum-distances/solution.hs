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

-- what shall i do about the result
-- no global variables in Hskell
l1 sa a r = map (\c -> setResult r rv c cv result)  [r..(sa-1)]
  where rv = a!!r
        cv = a!!c

-- solve sa a = l1 sa 0 (a!!0) (-1)
solve sa a = map (\r -> l1 sa a r ) [0..(sa-1)]

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    print $ solve n a
