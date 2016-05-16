import Data.List
import Data.Array
import Data.Traversable
import Control.Monad

str2ints :: String -> [Int]
str2ints s = map (\x -> read x) (words s)

tuplify :: [String] -> [(Int, Int)]
tuplify zz = map (\x -> (x !! 0, x !! 1)) (map str2ints zz)

findminx :: [(Int, Int)] -> Int
findminx a = minimum $ map fst a

findminy :: [(Int, Int)] -> Int
findminy a = minimum $ map snd a

findmaxx :: [(Int, Int)] -> Int
findmaxx a = maximum $ map fst a

findmaxy :: [(Int, Int)] -> Int
findmaxy a = maximum $ map snd a

doit :: [(Int, Int)] -> IO()
doit ii = do
  print (findminx ii, findminy ii, findmaxx ii, findmaxy ii ,"\n",ii)


main :: IO()
main = do
  nlines <- getLine
  inputs <- replicateM (read  nlines :: Int) getLine
  doit (tuplify inputs)
