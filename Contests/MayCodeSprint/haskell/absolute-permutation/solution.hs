import Data.List
import Data.Char
import Data.Function
import Data.List
import Control.Monad
import Text.Printf

splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

strf :: Int -> Int -> String
strf a b = printf "%d %d" a b


calculate x k = (fst x, frm)
  where
    posi = fst x
    i = snd x
    frm = abs (posi - i) == k

checkAbsolute x k = map (\ z ->  calculate z k) x

-- nailed the results
solveMe n k = map (\x -> ( result x) ) checked
  where p = permutations [1 .. n]
        ii = [1 .. n]
        checked = map (\ x -> checkAbsolute (zip x ii) k) p
        filterTrue x = map snd x
        filterPermutations x = map fst x
        alltrue x = all (\ x -> x == True) (filterTrue x)
        result x = if (alltrue x) then (filterPermutations x) else []


--solve :: [Int] -> [Int]
solve x = alt
  where n = x !! 0
        k = x !! 1
        searchResults = solveMe n k
        alt = filter (\ x -> x /= []) searchResults

main :: IO()
main = do
  tc <- getLine
  inputs <- replicateM (read  tc) getLine
  let zzz = map (\ x -> splitAndRead x) inputs
  -- print zzz
  let rrr = map (\ x -> solve x) zzz
  print rrr
