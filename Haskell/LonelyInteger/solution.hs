import Data.List
import Debug.Trace
-- very good writeup
-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html

get2Lines :: IO (String, String)
get2Lines = do
  print "enter line 1"
  line1 <- getLine
  print "enter line 2"
  line2 <- getLine
  return (line1, line2)

main :: IO ()
main = do
  (val1, val2) <- get2Lines
  let va = (read val1 :: Int)
  let vb = sort (map read (words val2) :: [Int])


  print va
  print vb
