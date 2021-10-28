-- solution
import Data.List

       -- not used
combinations k ns = filter ((k==).length) $ subsequences ns

powers nx p = map (^p) [1..nx]

solution a b = length (filter (\x-> x== a) (map sum (subsequences (powers 1000 b))))


readData :: IO (Integer, Integer)
readData = do
  l1 <- getLine
  l2 <- getLine
  return (read l1, read l2)

main = do
  dat <- readData
  putStrLn (show (solution (fst dat) (snd dat)))
