import Data.List
import Data.Char
import Data.Function
import Control.Monad
import Text.Printf

solve [] [] asc bsc = (asc, bsc)
solve (a:as) (b:bs) asc bsc
  | a > b = solve as bs (asc + 1) bsc
  | a < b = solve as bs asc (bsc + 1)
  | otherwise = solve as bs asc bsc

splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)



strf :: Int -> Int -> String
strf a b = printf "%d %d" a b

main :: IO()
main = do
  l1 <- getLine
  l2 <- getLine
  let nk = splitAndRead l1
  let num = map (\x -> digitToInt x) l2
  print (nk, num)
