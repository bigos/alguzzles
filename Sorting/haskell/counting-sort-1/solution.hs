module Main where

import Data.Array
import Data.List
import System.Environment
import System.IO

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Integer
    arrTemp <- getLine
    let result1 = elems $ accumArray (+) 0 (0,n-1) $ (map (\x->(x,1)) (map read (words arrTemp) :: [Integer]))
    let result = unwords $ map show result1
    hPutStrLn fptr result
    hFlush fptr
    hClose fptr
