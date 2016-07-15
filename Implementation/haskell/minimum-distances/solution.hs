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
-- no global variables in Haskell
l1 sa a r = foldl (\result c -> setResult r (a!!r) c (a!!c) result) (-1) [r..(sa-1)]

solve :: Int -> [Int] -> Int
solve sa a = minpos
  where positives = filter (\x -> x>0) mydata
        mydata = map (\r -> l1 sa a r ) [0..(sa-1)]
        minpos = if positives == [] then (-1) else minimum positives

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    print $ solve n a
