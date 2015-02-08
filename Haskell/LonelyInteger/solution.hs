main = do
  val1 <- getLine
  val2 <- getLine
  print (read val1 :: Int)
  print (map read (words val2) :: [Int])
