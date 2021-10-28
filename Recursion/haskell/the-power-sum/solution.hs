-- solution
import Data.List

--        -- not used
-- combinations k ns = filter ((k==).length) $ subsequences ns

-- powers nx p = filter (<= nx) $ map (^p) (take nx [1,2..])

-- solution a b = length sx
--   where sx = filter (\x -> sum x == a) $ subsequences (powers a b)


-- readData = do
--   l1 <- getLine
--   l2 <- getLine
--   return (read l1, read l2)

-- main = do
--   dat <- readData
--   putStrLn (show (solution (fst dat) (snd dat)))

tryPowers _ _ [] = 0
tryPowers number power (x:xs) | number < 0 = 0
tryPowers number power (x:xs) | number == x^power = 1 + (tryPowers number power xs)
tryPowers number power (x:xs) = (tryPowers (number-x^power) power xs) + (tryPowers number power xs)

main=do
  number <- readLn :: IO Int
  power <- readLn :: IO Int
  putStrLn $ show $ tryPowers number power [1..ceiling $ sqrt $ fromIntegral number]
