import Data.List
import Data.Array

binarySearch haystack needle lo hi
  | hi < lo = Nothing
  | pivot == needle = Just mid
  | (mid-1) < 1 = Nothing
  | (pivot > needle && prevpivot < needle) = Just (mid-1)
  | pivot > needle = binarySearch haystack needle lo (mid-1)
  | pivot < needle = binarySearch haystack needle (mid+1) hi
  | otherwise = Just mid
  where
    mid = lo + (hi-lo) `div` 2
    pivot = haystack!mid
    prevpivot = haystack!(mid-1)

solveMe :: ( Array Int Int, Int) -> IO ()
solveMe ( a, s) = do
  print a
  print s
  print "+++++"

-- IO because we read from stdin after parsing arguments
process :: (Int, [Int], Int) -> IO ()
process (n, a, tc) = do
  -- monadic code to read subsequent lines
  -- first \ _ because we discard value of [1 .. tc] and use it only for number
  -- of iterations
  mapM_ (\ _ -> getLine >>=
                \ rl ->
                  solveMe (added, (read rl :: Int)) )
    [1 .. tc]
  where added = array (1, n) (zip [1 .. n] (scanl1 (+) a))


getData :: IO (Int, [Int], Int)
getData = do
  n <- getLine
  a <- getLine
  tc <- getLine
  return (read n,
          reverse $ sort (  map (\x -> read x :: Int) (words a)), -- read space separated string as array of ints
          read tc)

main :: IO ()
main = do
  inputArgs <- getData
  process inputArgs

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
