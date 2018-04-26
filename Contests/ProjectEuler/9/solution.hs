-- solution
import Control.Monad

-- precompute filter (\[a,b,c] -> a^2 + b^2 == c^2) (comb 3 [1..3000])

hhh n = filter (\x-> (floor x) == (ceiling x)) (ggg n)

ggg n = map (\x -> sqrt ( n^2 - x^2)) [ n-1, (n-2) .. (n / 2)]


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
