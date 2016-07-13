import Control.Applicative
import Control.Monad
import System.IO
import Debug.Trace
import Data.Array

solve :: [Int] -> Int
solve x = trace ("tracing " ++ show x) $  x!!1
  where lx = length x
        arr = array (lx, lx) [(a, b) | a <- x, b <- x]

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    print $ solve a
