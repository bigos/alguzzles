-- solution
import Data.List

-- not used here
comb :: Int -> [a] -> [[a]]
comb m xs = combsBySize xs !! m
 where
   combsBySize = foldr f ([[]] : repeat [])
   f x next = zipWith (++) (map (map (x:)) ([]:next)) next

totNum :: Integer -> Integer -> Integer -> Integer
totNum x n a
  | a^n < x = (totNum x n (succ a)) + (totNum (x-a^n) n (succ a))
  | a^n == x = 1
  | otherwise = 0

solution a b = totNum a b 1

readData :: IO (Integer, Integer)
readData = do
  l1 <- getLine
  l2 <- getLine
  return (read l1, read l2)

main = do
  dat <- readData
  putStrLn (show (solution (fst dat) (snd dat)))
