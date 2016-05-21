import Data.List
import Data.Function
import Control.Monad

-- same as inputs in main
exdata = [["10 10","1 1 1"],["5 9","2 3 4"],["3 6","9 1 1"],["7 7","4 2 1"],["3 3","1 9 2"]]
-- hacking progress
-- λ> map (\ x -> (splitAndRead $ x !! 0, splitAndRead $ x !! 1, "   ") ) exdata

splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

prepare inputs = map (\ x -> (input1 x, input2 x)) inputs
  where
    input1 x = splitAndRead $ x !! 0
    input2 x = splitAndRead $ x !! 1

main :: IO()
main = do
  tc <- getLine
  inputs <- replicateM (read  tc) (replicateM 2 getLine)
  print inputs
