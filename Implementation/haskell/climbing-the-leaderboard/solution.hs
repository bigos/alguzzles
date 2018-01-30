-- climbing the leaderboard

import Debug.Trace

leader :: [Int] -> [Int] -> Int -> Int -> [Int] -> [Int]
leader ss rs prev pl a = --trace ("--- " ++ show (ss,rs,"--",prev,pl,a)) $
  if rs == []
  then a
  else
    if ss == []
    then leader ss (tail rs) prev (1+pl) (pl:a)
    else
      if prev == head ss
      then  leader (tail ss) rs (head ss) pl a
      else
        if head rs >= head ss
        then  leader ss (tail rs) (head ss) (1+pl) (pl:a)
        else  leader (tail ss) rs (head ss) (1+pl) a

solve _ ss _ as = (leader ss (reverse as) (1 + (head (reverse as))) 1 [])

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
