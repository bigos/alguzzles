-- solve :: Int -> [Int] -> Int -> (Int,[Int],Int)
-- solve n a s = (n, a, s)


getData :: IO (Int, [Int], Int, [Int])
getData = do
  n <- readLn :: IO Int
  a <- readLn :: IO [Int]
  tc <- readLn :: IO Int
  as <- readLn :: IO [Int]
  return (n, a, tc, as)

main :: IO ()
main = do
  inputs <- getData
  print inputs
