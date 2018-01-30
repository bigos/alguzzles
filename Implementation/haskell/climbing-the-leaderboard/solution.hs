-- climbing the leaderboard

import Debug.Trace

remdup :: [Int] -> [Int] -> [Int]
remdup [] aa = reverse aa
remdup ee aa = remdup (tail ee) (if aa/= [] && (head ee)==(head aa) then aa else ((head ee):aa))

unq l = remdup l []

leader :: [Int] -> [Int] -> Int -> [Int] -> [Int]
leader [] (r:rs) pl aa = leader [r] rs (0+pl) (pl:aa)
leader _ [] _ aa = aa
leader (s:ss) (r:rs) pl az
  | s >  r =  leader ss (r:rs) (1+pl) (az)
  | s == r =  leader ss rs     (1+pl) (pl:az)
  | s <  r =  leader (s:ss) rs (0+pl) (pl:az)

solve _ ss _ as = (leader (unq ss) (reverse as) 1 [])

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

zzz = solve 7 [100, 100, 50, 40, 40, 20, 10] 4 [4,5,5, 25, 50, 120, 120]

qqq = remdup [100, 100, 50, 40, 40, 20, 10] []
