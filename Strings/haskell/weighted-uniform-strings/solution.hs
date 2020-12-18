module Main where
-- https://www.hackerrank.com/challenges/weighted-uniform-string/problem

import Data.List
import Data.Maybe
import Debug.Trace

theInput = "abccddde\n6\n1\n3\n12\n5\n9\n10\n"

huh1 = groupSame [1,2,3] 0 [] []
huh2 = groupSame [1,2,2,2,3] 0 [] []
huh3 = groupSame [1,2,2,3,3,3,4,4,5,4,4,4,3,3,2,2,2,2] 0 [] []

groupSame seqn lastel sames acc = --trace (show (seqn,lastel,sames," acc ",acc)) $
  if null seqn
  then init endacc2
  else (if lastel == head seqn
        then groupSame nv (head seqn) (head seqn : sames) acc
        else groupSame nv (head seqn) [] endacc2)
  where
    nv = tail seqn
    endacc2 = if null sames
      then [lastel] : acc
      else (scanl (+) lastel  sames) : acc

chval :: Char -> Integer
chval ch = res
  where
    lu = find (\(k,_) -> k == ch) (zip ['a'..'z'] [1..])
    res = case lu of
      Nothing -> -1
      Just n -> snd n

solution s qn dd = --trace (show (s, qn, dd)) $
  unlines $ yesno results
  where
    ans = map chval s
    rns = concat $ groupSame ans 0 [] []
    gg = map maybebool $ map (\n -> find ( == n) rns) dd
    --aa = trace (show ("AAAAAAAAAAAAAa", gg)) $ 1
    results = gg

maybebool v = case v of
  Nothing -> False
  Just n -> True

yesno bb =  map (\b -> if b then "Yes" else "No") bb

tri1 (a, _, _) = a
tri2 (_, a, _) = a
tri3 (_, _, a) = a

mydata :: String -> (String, Int, [Integer])
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
