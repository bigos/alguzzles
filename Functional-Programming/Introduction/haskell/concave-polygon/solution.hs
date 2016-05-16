import Data.List
import Data.Array
import Data.Traversable
import Control.Monad


main :: IO()
main = do
  nlines <- getLine
  inputs <- replicateM (read  nlines :: Int) getLine
  print inputs
