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

-- solveMe :: Int -> Int -> a
-- solveMe n k = map (\ x -> checkAbsolute (zip x [1 .. n]) k) p
--     where p = permutations [1 .. n]
--           filterTrue x = map snd x
--           filterPermutations x = map fst x
--           alltrue x = all (\ x -> x == True) (filterTrue x)
--           result x = if (alltrue x) then (filterPermutations x) else [-1]

solveMe :: Int -> Int -> IO()
solveMe n k = do
  print n


printRec [] _ = putStrLn ""
printRec (x:xs) sep = do
  putStr sep
  putStr (show x)
  printRec xs " "


printAR [] = putStrLn "-1"
printAR a  = printRec ( head a) ""

main :: IO()
main = do
  tc <- getLine
  inputs <- replicateM (read  tc) getLine
  let zzz = map (\ x -> splitAndRead x) inputs
  mapM_ (\ x -> solveMe x!!0 x!!1) zzz
