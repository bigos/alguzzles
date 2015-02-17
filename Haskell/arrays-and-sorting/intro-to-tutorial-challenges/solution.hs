import Data.List
import Data.Maybe

main :: IO ()
main = do
  v <- readLn :: IO Int
  n <- readLn :: IO Int
  -- read the string, chop it and read ints
  arstr <- getLine
  let arl = words arstr
  let ar = map read arl :: [Int]
  print (fromJust (elemIndex v ar))
