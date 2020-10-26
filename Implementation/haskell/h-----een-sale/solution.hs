module Main where

import System.IO

solve2 :: Int -> Int -> Int -> Int  -> Int -> Int
solve2    p      d      m      s       acc =
  if endCond then retVal else again
  where
    endCond   = s < p
    retVal = if acc > 0 then acc - 1 else 0
    again = solve2 newp d m (s-p) (1 + acc)
    priceDesc = p > m
    newp = if priceDesc then p - d else p


solve :: [Int] -> Int
solve (p:d:m:s:[]) = solve2 p d m s 0


try = solve2 20 3 6 80 0

main :: IO ()
main = do
  let fromStdin = True --True or False
  handle <- if fromStdin then pure stdin else openFile "./input0.txt" ReadMode
  ln <- hGetLine handle
  let nums = (map (\x -> read x::Int) (words ln))
  print (solve nums)
  hClose handle
