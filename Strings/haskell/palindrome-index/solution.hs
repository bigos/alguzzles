import Debug.Trace
import Prelude
import Control.Monad

-- version with trace
-- palindrome a = trace ("print a:  " ++ show a) $ reverse a == a
palindrome a = reverse a == a

palRanges :: String -> [(Int, Int)]
palRanges s = (zip [0..h1] ( reverse [h2..k]))
  where k = (length s) - 1
        h1 = (div (length s) 2) - 1
        h2 = k - h1

palDiff s = map (\(x,y) -> if (s!!x == s!!y ) then (False, x, y) else (True, x, y)) (palRanges s)

palFilt s = filter (\(r,x, y) -> r == True) (palDiff s)

palCandidate s = map (\(r,x,y) -> max (pal x)  (pal y) ) (palFilt s)
  where pal sx = if (palindrome (tryRemove s sx)) then sx else (-sx)

palResult s = head $ filter (\x -> x >= 0) (palCandidate s)

stringRange s = [0 .. (length s - 1)]

-- tryRemove x i = (take i x) ++ (drop (i+1) x)
tryRemove x i = (fst d) ++ (drop 1 (snd d))
  where d = splitAt i x

stringReversed :: String -> (String, String)
stringReversed s = (s, reverse s)

stringsSplit (s1, s2) x = trace ("string split ") $ (tryRemove s1 x, tryRemove s2 ((length s1) - x - 1))

isPalindromeAt (s1, s2) i = nn
  where ss = stringsSplit (s1, s2) i
        nn = if ((fst ss) == (snd ss)) then i else (0 - i - 100)

solveMe x l = head $ filter (\y -> y >= 0) $ map (\y -> isPalindromeAt x y) l

solve x = if palindrome x then (-1) else ng
  where nx = solveMe (stringReversed x) (stringRange x)
        ng = palResult x

main :: IO ()
main = do
  tcx <- getLine
  let tc = read tcx
  replicateM_ tc (getLine >>= \myLine -> print $ solve myLine)

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
