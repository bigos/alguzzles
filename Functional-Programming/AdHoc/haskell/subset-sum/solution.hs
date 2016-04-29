import Data.List
import Data.Array

binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low,high)
  | high < low = Nothing
  | otherwise =
      let mid = (low + high) `div` 2 in
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

binarySearchArray :: (Ix i, Integral i, Ord e) => Array i e -> e -> Maybe i
binarySearchArray a x = binarySearch p (bounds a) where
  p m = x `compare` (a ! m)

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
