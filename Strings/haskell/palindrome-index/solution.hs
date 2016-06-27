import Debug.Trace
import Prelude

-- version with trace
-- palindrome a = trace ("print a:  " ++ show a) $ reverse a == a
palindrome a = reverse a == a

stringRange s = [0 .. (length s - 1)]

siftRemoved :: Int -> String -> String -> String -> Int
siftRemoved i h _ []
  | palindrome h = i
  | otherwise = 0-1
siftRemoved i h o r
  | palindrome (h ++ r) = i
  | otherwise = siftRemoved (i+1) (h ++ o) [head r] (tail r)

siftMe s = siftRemoved 0 [] [] s

tryRemove x i = (take i x) ++ (drop (i+1) x)

solveMe x l = head $ filter (\ y -> y > (-2)) indexes
  where
    indexes = map (\a -> if (palindrome (tryRemove x a)) then a else (-9)) l

solve x = if palindrome x then (-1) else solveMe x (stringRange x)

printRes :: Int -> IO()
printRes x = do
  print x

main :: IO ()
main = do
  mydata <- getContents
  let mylines = lines mydata
  let tc = read (head mylines) ::Int
  mapM_ (\ x -> printRes (solve x)) (tail mylines )

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
