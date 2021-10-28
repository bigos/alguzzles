-- solution
import Data.List

       -- not used
combinations k ns = filter ((k==).length) $ subsequences ns

powers nx p = filter (<= nx) $ map (^p) (take nx [1,2..])

solution a b = length (filter (== a) sx)
  where sx = (map sum (subsequences (powers a b)))

readData = do
  l1 <- getLine
  l2 <- getLine
  return (read l1, read l2)

main = do
  dat <- readData
  putStrLn (show (solution (fst dat) (snd dat)))
