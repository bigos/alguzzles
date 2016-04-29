import Data.List
import Data.Array

binarySearch haystack needle lo hi
  | hi < lo = (-1)
  | pivot == needle = mid
  | (mid-1) < 1 = (-1)
  | (pivot > needle && prevpivot < needle) = (mid-1)
  | pivot > needle = binarySearch haystack needle lo (mid-1)
  | pivot < needle = binarySearch haystack needle (mid+1) hi
  | otherwise = mid
  where
    mid = lo + (hi-lo) `div` 2
    pivot = haystack!mid
    prevpivot = haystack!(mid-1)

solveMe :: ( Int, Array Int Int, Int) -> IO ()
solveMe (n, a, s) = do
  print  (binarySearch a s 1 n)


-- IO because we read from stdin after parsing arguments
process :: (Int, [Int], Int) -> IO ()
process (n, a, tc) = do
  -- monadic code to read subsequent lines
  -- first \ _ because we discard value of [1 .. tc] and use it only for number
  -- of iterations
  mapM_ (\ _ -> getLine >>=
                \ rl ->
                  solveMe (n, added, (read rl :: Int)) )
    [1 .. tc]
  where added = array (1, n) (zip [1 .. n] (scanl1 (+) a))


getData :: IO (Int, [Int], Int)
getData = do
  n <- getLine
  a <- getLine
  tc <- getLine
  return (read n,
          sort (  map (\x -> read x :: Int) (words a)), -- read space separated string as array of ints
          read tc)

main :: IO ()
main = do
  inputArgs <- getData
  process inputArgs

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
