import Data.List
import Data.Array
import Data.Traversable
import Control.Monad

str2ints :: String -> [Int]
str2ints s = map (\x -> read x) (words s)

main :: IO()
main = do
  nlines <- getLine
  inputs <- replicateM (read  nlines :: Int) getLine
  print (map (\x -> str2ints x) inputs)
