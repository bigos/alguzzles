process :: (Int, [Int], Int) -> IO ()
process (n, a, tc) = do
  print (n,a,tc)
  print a
  print tc
  print "-- monadic code to read subsequent lines --"
  mapM_ (\y -> getLine >>= \rl -> print (y, rl) )
    [1 .. tc]


getData :: IO (Int, [Int], Int)
getData = do
  n <- getLine
  a <- getLine
  tc <- getLine
  return (read n,
          map (\x -> read x :: Int) (words a), -- read space separated string as array of ints
          read tc)

main :: IO ()
main = do
  inputArgs <- getData
  process inputArgs

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation

-- Haskell input output
-- http://book.realworldhaskell.org/read/io.html
