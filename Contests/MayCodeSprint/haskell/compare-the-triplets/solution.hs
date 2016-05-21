import Data.List
import Data.Function
import Control.Monad


splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

main :: IO()
main = do
  l1 <- getLine
  l2 <- getLine
  print $ solve [splitAndRead l1, splitAndRead l2]
