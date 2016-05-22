import Data.List
import Data.Char
import Data.Function
import Control.Monad
import Text.Printf

splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

strf :: Int -> Int -> String
strf a b = printf "%d %d" a b


--solveMe :: Int -> Int -> [Int]
solveMe a b = [b,a]

--solve :: [Int] -> [Int]
solve x = solveMe a b
  where a = x !! 0
        b = x !! 1

main :: IO()
main = do
  tc <- getLine
  inputs <- replicateM (read  tc) getLine
  let zzz = map (\ x -> splitAndRead x) inputs
  -- print zzz
  let rrr = map (\ x -> solve x) zzz
  print rrr

sampledata = [[2,1],[3,0],[3,2]] -- zzz
