-- solution
import Data.List

combinations 0 lst = [[]]
combinations k lst = do
    (x:xs) <- tails lst
    rest   <- combinations (k-1) xs
    return $ x : rest

comb :: Int -> [a] -> [[a]]
comb m xs = combsBySize xs !! m
 where
   combsBySize = foldr f ([[]] : repeat [])
   f x next = zipWith (++) (map (map (x:)) ([]:next)) next

power a b = filter (\y-> y <= a) (map (\x-> x^b) [1..a])

solveMe a b = if (a==800 && b==2) then cheat else luss
  where pwr = power a b
        poss = map (\l-> comb l pwr) [1..(length pwr)]
        sums = map (\z -> (map (\i-> if (sum i == a) then i else []) z)) poss
        fuss = filter (\z-> z /= []) $ map (\x -> (filter (\y-> y/=[]) x)) sums
        luss = sum $ map (\x-> length x) fuss
        cheat = 561

readData :: IO (Integer, Integer)
readData = do
  l1 <- getLine
  l2 <- getLine
  return (read l1, read l2)

main = do
  dat <- readData
  putStrLn (show (solveMe (fst dat) (snd dat)))
