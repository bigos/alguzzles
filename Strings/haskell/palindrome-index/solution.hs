import Prelude
import Data.Map (toList, fromListWith)

palindrome a = reverse a == a

stringRange s = [0 .. (length s - 1)]

                -- inefficient
-- tryRemove x i = (take i x) ++ (drop (i+1) x)
tryRemove x i = (fst d) ++ (drop 1 (snd d))
  where d = splitAt i x

solveMe x l = if indexes == []
  then (-1)
  else head $ filter (\ y -> y > (-2)) indexes
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
