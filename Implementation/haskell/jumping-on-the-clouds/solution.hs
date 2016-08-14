import Data.Char

main :: IO()
main = do
  n0 <- getLine
  s0 <- getLine
  let n = read n0 :: Int
  let s =  map read $ words s0 :: [Int]
  print (n,s)
