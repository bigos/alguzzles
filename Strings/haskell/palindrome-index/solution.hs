import Debug.Trace
import Prelude

-- version with trace
-- palindrome a = trace ("print a:  " ++ show a) $ reverse a == a
palindrome a = reverse a == a

stringRange s = [0 .. (length s - 1)]



-- tryRemove x i = (take i x) ++ (drop (i+1) x)
tryRemove x i = (fst d) ++ (drop 1 (snd d))
  where d = splitAt i x

stringReversed :: String -> (String, String)
stringReversed s = (s, reverse s)

stringsSplit (s1, s2) x = (tryRemove s1 x, tryRemove s2 ((length s1) - x - 1))

isPalindromeAt s i = (fst ss) == (snd ss)
  where ss = stringsSplit (stringReversed s) i

solveMe x l = map (\y -> sa y) l
  where sa z = stringsSplit (stringReversed x) z

--solve x = if palindrome x then (-1) else solveMe x (stringRange x)

printRes :: Int -> IO()
printRes x = do
  print x

main :: IO ()
main = do
  mydata <- getContents
  let mylines = lines mydata
  let tc = read (head mylines) ::Int
  print tc

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
