module Main where
-- https://www.hackerrank.com/challenges/weighted-uniform-string/problem

import Data.List
import Data.Maybe

theInput = "abccddde\n6\n1\n3\n12\n5\n9\n10\n"

solution s qn dd = unlines $ yesno results
  where results = map odd [1..5]

groupWeights s = group $ map doChar $ tri1 $ mydata s

doChar c = fromMaybe (-1) $ lookup c (zip ['a'..'z'] [1..26])

yesno bb =  map (\b -> if b then "Yes" else "No") bb

tri1 (a, _, _) = a
tri2 (_, a, _) = a
tri3 (_, _, a) = a

mydata :: String -> (String, Int, [Int])
mydata ll = (s, cnt, dat)
  where (s : c : d) = lines ll
        cnt = read c
        dat = map read d

experiment = solution s q d
  where a = mydata theInput
        s = tri1 a
        q = tri2 a
        d = tri3 a

main :: IO ()
main = do
  cont <- getContents
  let d  = mydata cont
  let s  = tri1 d
  let qn = tri2 d
  let dd = tri3 d
  putStrLn $ solution s qn dd
