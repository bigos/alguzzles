module Main where

import Debug.Trace
import System.IO

solve2 :: Int -> Int -> Int -> Int  -> Int -> Int
solve2    p      d      m      s       acc =
  trace ("===== pdmsacc " ++ show (p, d, m, s, acc)) $
  if endCond then acc else solve2 newp d m (s-p) (1 + acc)
  where
    endCond   = s < p
    newp = max m (p - d)

solve :: [Int] -> Int
solve (p:d:m:s:[]) = solve2 p d m s 0


try1 = solve2 20 3 6 80 0
try2 = solve2 20 3 6 85 0


main :: IO ()
main = do
  let fromStdin = True --True or False
  handle <- if fromStdin then pure stdin else openFile "./input0.txt" ReadMode
  ln <- hGetLine handle
  let nums = (map (\x -> read x::Int) (words ln))
  print (solve nums)
  hClose handle
