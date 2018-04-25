-- solution
import Control.Monad

pythagor :: Integer -> Integer
pythagor n = -1

readInteger :: IO Integer
readInteger = getLine >>= (\s -> return (read s :: Integer))

readNUmbers n = forM [1..n] (\_ -> readInteger)

main :: IO ()
main = do
  ri <- readInteger
  -- putStrLn ("read "++ (show ri))
  zz <- readNUmbers ri
  mapM_ (\n -> putStrLn (show (pythagor n))) zz
