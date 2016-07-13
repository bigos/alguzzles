import Debug.Trace
import Prelude
import Control.Monad

-- version with trace
-- palindrome a = trace ("print a:  " ++ show a) $ reverse a == a
palindrome a = reverse a == a

palRanges :: String -> [(Int, Int)]
palRanges s = (zip [0..h1] ( reverse [h2..k]))
  where lns = length s
        k = (lns) - 1
        h1 = (div (lns) 2) - 1
        h2 = k - h1

-- this needs fixing
-- !! are slow,
palDiff s = map (\(x,y) -> if (s!!x == s!!y ) then (False, x, y) else (True, x, y)) (palRanges s)

palFilt s = filter (\(r, x, y) -> r == True) (palDiff s)

palCandidate s = map (\(_, x, y) -> max (pal x)  (pal y) ) (palFilt s)
  where pal sx = if (palindrome (tryRemove s sx)) then sx else ((-sx) - 1)

palResult s = head $ filter (\x -> x >= 0) (palCandidate s)

-- tryRemove x i = (take i x) ++ (drop (i+1) x)
tryRemove x i = (fst d) ++ (drop 1 (snd d))
  where d = splitAt i x

solve x = if palindrome x then (-1) else ng
  where ng = palResult x

main :: IO ()
main = do
  tcx <- getLine
  let tc = read tcx
  replicateM_ tc (getLine >>= \myLine -> print $ solve myLine)

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
