import Control.Applicative
import Control.Monad
import System.IO
import Debug.Trace

solve :: [Int] -> Int
solve x = trace ("tracing " ++ show x) $  x!!1

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    print $ solve a
