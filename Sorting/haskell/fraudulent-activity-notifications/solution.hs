-- solution.hs

module Main where

import Data.List

medianFromSorted :: Fractional a => [a] -> Maybe a
medianFromSorted [] = Nothing
medianFromSorted [a] = Just a
medianFromSorted [a,b] = Just ((a + b) / 2)
medianFromSorted (a:xs) = medianFromSorted (init xs) -- init is not efficient

median :: Ord a => Fractional a => [a] -> Maybe a
median = medianFromSorted . sort

nc :: Integer -> [Integer] -> Integer -> Integer
nc hr h2 count =
  case mv of
    Nothing -> count
    Just x  -> if hr >= x then (count + 1) else count
  where mv = median (map fromIntegral h2)

recurse :: Integer -> Integer -> [Integer] -> [Integer] -> Integer -> Integer
recurse n d [] remainingE count = recurse    n d [] (drop 1 remainingE) count
recurse n d seenE [] count = nc 0 [] count
recurse n d seenE remainingE count = recurse n d h2 (drop 1 remainingE) (nc hr h2 count)
  where hr = head remainingE
        lastSeen = take (fromIntegral (d)) seenE
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
