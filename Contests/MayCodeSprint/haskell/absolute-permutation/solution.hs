import Data.List
import Data.Char
import Data.Function
import Control.Monad
import Text.Printf

splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

strf :: Int -> Int -> String
strf a b = printf "%d %d" a b



main :: IO()
main = do
  tc <- getLine
  inputs <- replicateM (read  tc) getLine
  let zzz = map (\ x -> splitAndRead x) inputs
  print zzz
