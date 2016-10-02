-- solution goes here
import Control.Monad

sharedItems :: [Int] -> [Int] -> [Int]
sharedItems nk costs = init ++ fini
  where n = head nk
        k = last nk
        init = take k costs
        fini = drop (k+1) costs

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
  let sharedCost = sum (sharedItems nk0 costs)
  let personCost = div sharedCost 2
  let result = charged - personCost
  if result == 0 then print "Bon Appetit" else print result
