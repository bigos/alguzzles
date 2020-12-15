{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where
# https://www.hackerrank.com/challenges/weighted-uniform-string/problem

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the weightedUniformStrings function below.
weightedUniformStrings s queries = do

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    queriesCount <- readLn :: IO Int

    queriesTemp <- readMultipleLinesAsStringArray queriesCount
    let queries = Data.List.map (read :: String -> Int) queriesTemp

    let result = weightedUniformStrings s queries

    hPutStrLn fptr $ intercalate "\n" result

    hFlush fptr
    hClose fptr
