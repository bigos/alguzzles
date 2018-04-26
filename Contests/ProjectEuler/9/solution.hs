-- solution
import Control.Monad

pytr :: Int -> [(Bool, Int, Int, Int)]
pytr n =
  filter
    (\(_, a, b, c) -> a + b + c <= n)
    [ (prim a b c, a, b, c)
    | a <- xs
    , b <- drop a xs
    , c <- drop b xs
    , a ^ 2 + b ^ 2 == c ^ 2 ]
  where
    xs = [1 .. n]
    prim a b _ = gcd a b == 1

pytr2 n = map (\(_,a,b,c) -> [a, b, c]) (pytr n)

sq n i = n!!i * n!!i

check :: [Integer] -> Bool
check n = (sq n 0 + (sq n 1) == (sq n 2))

fff n = filter check (comb 3 [1..n])

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb m (x:xs) = map (x:) (comb (m-1) xs) ++ comb m xs

pypypy n = filter check (fff n)

pythagoras n = map (\x -> if (n == (sum x)) then (product x) else (-1)) (fff n)

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
