import Data.List
-- import Debug.Trace

-- very good writeup
-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html

get2Lines :: IO (String, String)
get2Lines = do
  line1 <- getLine
  line2 <- getLine
  return (line1, line2)

main :: IO ()
main = do
  (val1, val2) <- get2Lines
  let n = (read val1 :: Int)
  let a = sort (map read (words val2) :: [Int])
  let res = filter (\e -> length (elemIndices e a) == 1) a
  print (head res)
