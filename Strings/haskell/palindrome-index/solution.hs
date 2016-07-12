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
stringReversed s = (s, reverse s)

stringsSplit (s1, s2) x = (tryRemove s1 x, tryRemove s2 ((length s1) - x - 1))

isPalindromeAt s i = nn
  where ss = stringsSplit (stringReversed s) i
        nn = if ((fst ss) == (snd ss)) then i else (i - 100)

solveMe x l = map (\y -> isPalindromeAt x y) l

solve x = if palindrome x then (-1) else nx
  where nx =  head $ filter (\y -> y >= 0)  $ solveMe x (stringRange x)

printRes :: Int -> IO()
printRes x = do
  print x

main :: IO ()
main = do
  tcx <- getLine
  let tc = read tcx
  replicateM_ tc (getLine >>= \myLine -> print $ take 1 myLine)


  -- mapM_ (\ x -> printRes (solve x)) (tail mylines )


-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
