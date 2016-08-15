consumeUntil :: [Int] -> Int -> Int
consumeUntil [] _ = (-1)
consumeUntil a n  = if head a >= n then (if head a == n then n else (0-n)) else consumeUntil (tail a) n

-- numbers in a seem to be subsequent with few gaps
solution :: Int -> [Int] -> Int -> Int
solution _ [] acc = acc
solution d a  acc = solution d (tail a) (if triplet then (acc + 1) else acc)
  where dpart = take (2 * d+1) a
        foundi = head a
        wantedj = foundi + d
        wantedk = foundi + 2 * d
        triplet = (consumeUntil dpart wantedj > 0) && (consumeUntil dpart wantedk > 0)

main :: IO()
main = do
  nd0 <- getLine
  a0  <- getLine
  let _ = read $ head $ words nd0 :: Int
  let d = read $ head $ tail $ words nd0 :: Int
  let a = map read (words a0) :: [Int]
  print $ solution d a 0
