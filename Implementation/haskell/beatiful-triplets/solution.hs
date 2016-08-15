solution :: Int -> [Int] -> Int -> Int
solution _ [] c = c
solution d a _  = length $ filter (\x -> ((x!!1 - x!!0) == d) && (x!!1 - x!!0) == (x!!2 - x!!1)) triplets
  where triplets = filter (\x -> x!!0 < x!!1 && x!!1 < x!!2) [[i,j,k] | i<-a, j<-a, k<-a]

main :: IO()
main = do
  nd0 <- getLine
  a0  <- getLine
  let _ = read $ head $ words nd0 :: Int
  let d = read $ head $ tail $ words nd0 :: Int
  let a = map read (words a0) :: [Int]
  print $ solution d a 0
