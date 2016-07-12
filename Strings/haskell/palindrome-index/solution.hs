import Debug.Trace
import Prelude
import Control.Monad

-- version with trace
-- palindrome a = trace ("print a:  " ++ show a) $ reverse a == a
palindrome a = reverse a == a

stringRange s = [0 .. (length s - 1)]



-- tryRemove x i = (take i x) ++ (drop (i+1) x)
tryRemove x i = (fst d) ++ (drop 1 (snd d))
  where d = splitAt i x

stringReversed :: String -> (String, String)
stringReversed s = trace ("reversing ") $ (s, reverse s)

stringsSplit (s1, s2) x = (tryRemove s1 x, tryRemove s2 ((length s1) - x - 1))

isPalindromeAt (s1, s2) i = nn
  where ss = stringsSplit (s1, s2) i
        nn = if ((fst ss) == (snd ss)) then i else (0 - i - 100)

solveMe x l = head $ filter (\y -> y >= 0) $ map (\y -> isPalindromeAt x y) l

solve x = if palindrome x then (-1) else nx
  where nx = solveMe (stringReversed x) (stringRange x)

main :: IO ()
main = do
  tcx <- getLine
  let tc = read tcx
  replicateM_ tc (getLine >>= \myLine -> print $ solve myLine)


  -- mapM_ (\ x -> printRes (solve x)) (tail mylines )


-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
