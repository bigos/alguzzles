zzz x = print (head  x)
main = do
  val1 <- getLine
  val2 <- getLine
  print (read val1 :: Int)
  print (map read (words val2) :: [Int])

  -- don't waste your time with functional programming go back to Lisp
