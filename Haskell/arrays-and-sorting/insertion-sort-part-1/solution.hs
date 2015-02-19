import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

-- 2468 (3) replace 3 with nothing if 3 < 8
-- 2468 ()
-- 2468 (8) 2468 ++ last of 2468 ++ ()
-- 246 (68) 246 ++ last of 246 ++ (8)
-- 24 (468) 24 ++ last of 24 ++ (68)
-- 2 (2468) replace 2 with 3 because 3 >= 2 ; 2 ++ last of 2 ++ (468)
-- 2 (3468) DONE

mybeg i ar = take i ar
myfin i ar = drop (i+1) ar

aiv i v ar = join [(mybeg i ar) , [v] , (myfin i ar)]

-- run like this: insort 5 3 3 [0,1,2,4,5,3]
insort i v iv ar
  | trace (show i ++ show v ++ show iv ++ show ar) False = undefined
  | trace ("  >"++show start++show finish++show niv) False = undefined
  | i < 1 = i
  | otherwise = insort (i-1) v niv (aiv i v ar)
  where start = take i ar
        finish = drop (i+1) ar
        niv = if i > 1 then ar !! (i-1) else (0)

main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let arwords = words arstr
  let ar = (map read arwords) :: [Int]
  print s
  print ar
