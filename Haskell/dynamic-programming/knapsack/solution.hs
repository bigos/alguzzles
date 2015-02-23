import Control.Monad
import Data.List

solve :: Int -> [Int] -> Int
solve k ar = 1

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
