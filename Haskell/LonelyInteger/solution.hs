-- very good writeup
-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html

isTheSame x y = x==y

get2Lines = do
    line1 <- getLine
    line2 <- getLine
    return (line1, line2)

main = do
  (val1, val2) <- get2Lines
  let va = (read val1 :: Int)
  let vb = (map read (words val2) :: [Int])

  print va
  print vb
