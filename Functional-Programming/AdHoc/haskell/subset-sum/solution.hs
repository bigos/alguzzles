solve :: (Int, [Int], Int, Int ) -> IO ()
solve (n, a, tc, as) = do
  print (n,a,tc,as)
  print a
  print tc
  print as


getData :: IO (Int, [Int], Int, Int)
getData = do
  n <- getLine
  a <- getLine
  tc <- getLine
  as <- getLine
  return (read n,
          map (\x -> read x :: Int) (words a), -- read space separated string as array of ints
          read tc,
          read as)

main :: IO ()
main = do
  inputArgs <- getData
  solve inputArgs
