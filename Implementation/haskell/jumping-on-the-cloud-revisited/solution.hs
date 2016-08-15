solution :: Int -> [Int] -> Int -> Int
solution _ [] a = a
solution k c a = solution k (drop k c) (a - loss)
  where loss = if (head c) == 1 then 3 else 1

main :: IO()
main = do
  nkx <- getLine
  cx  <- getLine
  let n = read $ head (words nkx) :: Int
  let k = read $ head $ tail (words nkx) :: Int
  let c = map read (words cx) :: [Int]
  -- print (n,k,c)
  print $ solution k c 100
