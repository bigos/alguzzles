import Data.List
import Data.Function
import Control.Monad
import Text.Printf

fibme :: Integer -> Integer -> [Integer] -> [Integer]
fibme lastTerm term dat
  | term > lastTerm = dat
  | otherwise = fibme lastTerm (term + 1) ( [formula] ++ dat)
  where formula = dat!!1 + (dat!!0)^2

splitAndRead :: String -> [Integer]
splitAndRead str = map read (words str)

strf :: Int -> String
strf a = printf "%d" a

main :: IO()
main = do
  l1 <- getLine
  let inputData = splitAndRead l1
  let a = inputData!!0
  let b = inputData!!1
  let n = inputData!!2
  let res = (fibme n 3 [b, a])
  print (head res)
