-- climbing the leaderboard

import Debug.Trace
import Data.Array
import Data.List

remdup :: [Int] -> [Int] -> [Int]
remdup [] aa = reverse aa
remdup (e:es) aa = remdup es (if aa/= [] && e==(head aa) then aa else (e:aa))

unq l = remdup l []

leader :: [Int] -> [Int] -> Int -> [Int] -> [Int]
leader _  [] _ aa = aa
leader [] (r:rs) pl aa = leader [] rs (if r== (head rs) then 0+pl else  0+pl) (pl:aa)
leader (s:ss) (r:rs) pl az
  | s >  r    =  leader ss (r:rs) (1+pl) (az)
  | otherwise =  leader (s:ss) rs (0+pl) (pl:az)
  --where dbg zn = trace ("***"++ zn ++ show ((s:ss),(r:rs),"--",pl,az))

solve _ ss _ as = (leader (unq ss) (reverse as)  1 [])

readNumber :: IO Int
readNumber = getLine >>= (\n -> return (read n))

readInts :: IO [Int]
readInts = getLine >>= (\ln -> return (map (\x -> read x) (words ln)))

main :: IO ()
main =
  readNumber         >>=
  (\n  -> readInts   >>=
  (\ss -> readNumber >>=
  (\m  -> readInts   >>=
  (\as -> mapM_ print (solve n ss m as)))))

zzz = solve 7 [100, 100, 50, 40, 40, 20, 10] 4 [5, 25, 50, 120]
aaa = solve 7 [100, 100, 50, 40, 40, 20, 10, 4, 3] 4 [5, 25, 50, 120]
qqq = solve 7 [100, 100, 50, 40, 40, 20, 10] 4 [1,3,3,5, 25, 50, 98]
www = solve 7 [100, 100, 98, 98, 97, 97] 4 [97, 97, 98, 98, 98,  99, 99,  120]
eee = solve 7 [100, 100, 98, 98, 97, 97,90,0] 4 [2,3,4,96,97, 97, 98, 98, 98,  99, 99,  120]
rrr = solve 7 [100, 100, 98, 98, 97, 97, 90] 4 [1,2,3,4,5,6,7]

-- figured out how to create arrays in Haskell
ary = array (0, 9) [(i-1,i) | i<- [1..10]]
-- and might use it for this imperative solution
-- https://hebr3.github.io/2017/09/hacker-rank-climbing-the-leaderboard.html
