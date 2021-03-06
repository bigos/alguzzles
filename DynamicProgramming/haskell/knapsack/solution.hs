import Control.Monad
import Data.List
import Debug.Trace

sumingredients k ar = join $ (map (\x -> (last (sequences (div k x) x) ) ) ar)
sequences :: Int -> Int -> [[Int]]
sequences max num = inits $ take max $ repeat num

solve :: Int -> [Int] -> Int
solve k ar
  | trace (show (k,ar) ) False = undefined
  | otherwise = maximum maxes
  where maxes = filter (\x -> x <= k)$ map sum $ subsequences $ sumingredients k ar

process :: (Int,Int,[Int]) -> String
process dat
  | otherwise = show (solve k ar)
  where (_, k, ar) = dat

getData :: IO  (Int, Int, [Int])
getData = do
  dd <- fmap (map read . words) getLine
  let n = head dd
  let k = last dd
  ar <- fmap (map read . words) getLine
  return (n, k, ar)

main :: IO ()
main = do
  t <- readLn :: IO Int
  inputs <- replicateM t getData
  mapM_ (\x -> putStrLn (process x)) inputs
