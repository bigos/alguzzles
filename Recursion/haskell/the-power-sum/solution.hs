-- solution
import Data.List

combinations 0 lst = [[]]
combinations k lst = do
    (x:xs) <- tails lst
    rest   <- combinations (k-1) xs
    return $ x : rest

solveMe a b = luss
  where pwr = filter (\y-> y<=a) (map (\x-> x^b) [1..a])
        poss = map (\l-> combinations l pwr) [1..(length pwr)]
        fuss = map (\z -> (filter (\y -> y == a) (map sum z))) poss
        luss = sum $ map (\x-> length x) fuss

readData :: IO (Integer, Integer)
readData = do
  l1 <- getLine
  l2 <- getLine
  return (read l1, read l2)

main = do
  dat <- readData
  putStrLn (show (solveMe (fst dat) (snd dat)))
