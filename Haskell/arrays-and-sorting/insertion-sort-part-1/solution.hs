import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

mybeg i ar = take i ar
myfin i ar = drop (i+1) ar

aiv i v ar = join [(mybeg i ar) , [v] , (myfin i ar)]

-- run like this: insort 5 3 3 [0,1,2,4,5,3]
insort i v iv ar
  | trace (show i ++ show v ++ show iv ++ show ar ++ "  >"++show start++show finish) False = undefined
  | i < 1 = i
  | otherwise = insort (i-1) v niv (aiv i v ar)
  where start = take i ar
        finish = drop (i+1) ar
        niv = ar !! (i-1)

main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let arwords = words arstr
  let ar = (map read arwords) :: [Int]
  print s
  print ar
