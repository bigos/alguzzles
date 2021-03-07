module Main where

import Data.List

pows :: Integer -> Integer -> [Integer]
pows n p = takeWhile (\x -> (x^p) <= n) [1..]

powcomb :: Integer -> Integer -> [[Integer]]
powcomb n p = subsequences (pows n p)

powr :: Integer -> Integer -> [[Integer]]
powr n p = map (map (^p)) (powcomb n p)

findRootCeil :: Integer -> Integer -> Int
findRootCeil n p = length (filter (\a -> sum a == n) (powr n p))

solve :: Integer -> Integer -> Int
solve = findRootCeil

main = do
  contents <- getContents
  --let contents = "10\n2"

  let x = read ((lines contents)!!0)::Integer
  let n = read ((lines contents)!!1)::Integer
  putStrLn  (show (solve x n))
