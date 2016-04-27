-- solve :: Int -> [Int] -> Int -> (Int,[Int],Int)
-- solve n a s = (n, a, s)
getData :: IO (Int, [Int], Int, Int)
getData = do
  n <- getLine
  a <- getLine
  tc <- getLine
  as <- getLine
  return (read n,
          map (\x -> read x :: Int) (words a), -- read space separated string as array of ints
          read tc, read as)

main :: IO ()
main = do
  inputStrings <- getData

  print inputStrings
