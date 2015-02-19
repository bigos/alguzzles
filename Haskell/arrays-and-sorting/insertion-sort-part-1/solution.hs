import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

-- 2468 (3) replace 3 with nothing if 3 < 8
-- 2468 ()
-- 2468 (8) 2468 ++ last of 2468 ++ () ;  sliced 3 [2,4,6,8]
-- 246 (68) 246 ++ last of 246 ++ (8)
-- 24 (468) 24 ++ last of 24 ++ (68)
-- 2 (2468) replace 2 with 3 because 3 >= 2 ; 2 ++ last of 2 ++ (468)
-- 2 (3468) DONE

mybeg i ar = take (i+1) ar
myfin i ar = drop i ar

sliced i ar = (mybeg i ar, myfin i ar)

disp done sl v = join [fst sl, if done then ([v]++ tail (snd sl)) else snd sl]

-- insort 3 3 [2,4,6,8]
insort :: Int -> Int -> [Int] -> [String] -> [String]
insort i v ar res
  | trace (concatMap show (disp done sl v)) False = undefined
  | trace (show res) False = undefined
  | i < 1 = res
  | otherwise = insort (i-1) v ar [res++(concatMap show (disp done sl v))]
  where sl = sliced i ar
        head2nd = (head $ snd sl)
        done = if head2nd <= v then True else False

main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let arwords = words arstr
  let ar = (map read arwords) :: [Int]
  print s
  print ar
  --print insort (length ar) (last ar) (init ar)
  print ""
