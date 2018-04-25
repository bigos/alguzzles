-- solution
import Control.Monad

sq n i = n!!i * n!!i

check :: [Integer] -> Bool
check n = (sq n 0 + (sq n 1) == (sq n 2))

mul x = head (reverse (scanl (\b a -> b*a) 1 x))

fff n = filter (\x -> check x) (comb 3 [1..n])

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb m (x:xs) = map (x:) (comb (m-1) xs) ++ comb m xs

pythagoras n = map (\x -> if (n==(sum x)) then (mul x) else (-1)) (fff n)

pythagor1 n = filter (\x -> x /= (-1)) (pythagoras n)

pythagor :: Integer -> Integer
pythagor n = if ((pythagor1 n) == []) then (-1) else (head (pythagor1 n))

readInteger :: IO Integer
readInteger = getLine >>= (\s -> return (read s :: Integer))

readNUmbers :: Integer -> IO [Integer]
readNUmbers n = forM [1..n] (\_ -> readInteger)

main :: IO ()
main = do
  ri <- readInteger
  -- putStrLn ("read "++ (show ri))
  zz <- readNUmbers ri
  mapM_ (\n -> putStrLn (show (pythagor n))) zz
