-- https://www.hackerrank.com/challenges/countingsort1/problem

module Main where

import Data.Array
import System.Environment (getEnv)
import System.IO

-- λ> accumArray (+) 0 (0,5) [(1,1),(1,1),(1,1),(1,1),(3,1),(3,1)]
-- array (0,5) [(0,0),(1,4),(2,0),(3,2),(4,0),(5,0)]

recsol :: Integer -> [Integer] -> Array Integer Integer -> Array Integer Integer
recsol n d ar =
  if null d
  then ar
  else recsol n (tail d) (ar // [(i, nv)])
  where
    i = head d
    nv = (ar!i) + 1

zeroed :: Integer ->  Array Integer Integer
zeroed n = array (0,n) [(i, 0) | i <- [0..n]]

solution :: Integer -> [Integer] -> String
solution n d = unwords $ map show (elems res)
  where
    res = recsol n d (zeroed n)

-- results are ok but problem with IO in main

main :: IO ()
main = do
  let fromStdin = True --must be True for submission or False for development
  handle <- if fromStdin then pure stdin else openFile "./input0.txt" ReadMode
  contents <- hGetContents handle
  let linez = lines contents
  let num = read (head linez) :: Integer
  let dat = map read (words (linez!!1)) :: [Integer]
  let !res = solution num dat
  hClose handle

  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  hPutStrLn fptr res
  hFlush fptr
  hClose fptr
