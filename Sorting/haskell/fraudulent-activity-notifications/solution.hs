-- solution.hs

module Main where

import Data.List

getMiddle [] = 0
getMiddle xs = (a' + b') `div` 2
    where a' = head $ drop a xs
          b' = head $ drop b xs
          a = (n `div` 2)
          b = n' - 1
          n' = n `div` 2
          n = length xs

median :: [Integer] -> Integer
median [] = 0
median xs = result
    where result = if (n `mod` 2 == 0)
                    then getMiddle sorted
                    else head $ drop a sorted
          a = (n - 1) `div` 2
          n = length xs
          sorted = sort xs

nc hr h2 count = if hr >= (median h2) then count + 1 else count

recurse :: Integer -> Integer -> [Integer] -> [Integer] -> Integer -> Integer
recurse n d [] remainingE count = recurse n d (take 1 remainingE) (drop 1 remainingE) count
recurse n d seenE [] count = count
recurse n d seenE remainingE count = recurse n d h2 (drop 1 remainingE) (nc hr h2 count)
  where hr = head remainingE
        lastSeen = take (fromIntegral d) seenE
        h2 = (hr : lastSeen)


solve2 :: Integer -> Integer -> [Integer] -> Integer
solve2 n d e = recurse n d [] e 0

solve :: String -> String
solve a = show (solve2 n d e)
  where
    lx = lines a
    l1 = head lx
    l2 = lx!!1
    l1n = map (\x -> read x::Integer) $ words l1
    l2n = map (\x -> read x::Integer) $ words l2
    n = head l1n
    d = l1n!!1
    e = l2n

dat = "5 4\n1 2 3 4 4"

main = interact solve
       -- unfinished puzzle
-- https://www.hackerrank.com/challenges/fraudulent-activity-notifications/problem?isFullScreen=true&h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=sorting
