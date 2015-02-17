import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace


my n v ar
  | trace (show v) False = undefined
  | otherwise = ss
  where vi = fromJust (elemIndex v ar)
        ss = splitAt vi ar

main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let arwords = words arstr
  let ar = (map read arwords) :: [Int]
  print s
  print ar
