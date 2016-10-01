-- solution goes here
import Control.Monad

splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

main :: IO()
main = do
  line1 <- getLine
  line2 <- getLine
  line3 <- getLine
  let nk0 = splitAndRead line1
  let costs = splitAndRead line2
  let charged = read line3 :: Int
  print (nk0, costs, charged)
