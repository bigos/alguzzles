-- solve :: Int -> [Int] -> Int -> (Int,[Int],Int)
-- solve n a s = (n, a, s)



getData = do
  n <- getLine
  a <- getLine
  tc <- getLine
  as <- getLine
  return (n, a, tc, as)

main :: IO ()
main = do
  inputStrings <- getData

  print inputStrings
