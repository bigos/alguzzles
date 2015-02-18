import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

mybeg i ar = take i ar
myfin i ar = drop (i+1) ar

aiv i v ar = join [(mybeg i ar) , [v] , (myfin i ar)]

insort i v ar
  | trace (show i ++ show v ++ show ar ++ "  >"++show start++show finish) False = undefined
  | i < 1 = i
  | otherwise = insort (i-1) v (aiv i v ar)
  where start = take i ar
        finish = drop (i+1) ar

-- myy n v ar
--   | trace (show v) False = undefined
--   | otherwise = (ss, vi, sr)
--   where vi = fromJust (elemIndex v ar)
--         ss = splitAt vi ar
--         firstlast = (last (fst ss))
--         secondfirst = (head (snd ss))
--         sr = if secondfirst >= firstlast then secondfirst else firstlast



main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let arwords = words arstr
  let ar = (map read arwords) :: [Int]
  print s
  print ar
