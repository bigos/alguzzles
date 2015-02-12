import Data.Bits

maxXor :: Int -> Int -> Int
maxXor l r = -- complete this function.

main :: IO ()
main = do
  l <- readLn
  r <- readLn
  print $ maxXor l r
