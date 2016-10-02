-- solution goes here
import Control.Monad

sharedItems :: [Int] -> [Int] -> [Int]
sharedItems nk costs = initi ++ finit
  where k = last nk
        initi = take k costs
        finit = drop (k+1) costs

solveMe :: [Int] -> Int -> Int
solveMe sc c = c - pc
  where pc = div ssc 2
        ssc = sum sc

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
  let sharedCosts = (sharedItems nk0 costs)
  let result = solveMe sharedCosts charged

  if result == 0 then putStr "Bon Appetit" else print result
