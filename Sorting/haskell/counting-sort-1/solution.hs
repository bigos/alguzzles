--{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

-- import Control.Monad
import Data.Array
-- import Data.Bits
import Data.List
-- import Data.Set
import System.Environment
import System.IO

-- λ> accumArray (+) 0 (0,5) [(1,1),(1,1),(1,1),(1,1),(3,1),(3,1)]
-- array (0,5) [(0,0),(1,4),(2,0),(3,2),(4,0),(5,0)]

recsol :: Integer -> [Integer] -> Array Integer Integer -> Array Integer Integer
recsol n d ar = if null d
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

-- Complete the countingSort function below.
countingSort _arr = undefined

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Integer

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . words $ arrTemp


    let result = solution (n-1) (map read (words arrTemp))

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
