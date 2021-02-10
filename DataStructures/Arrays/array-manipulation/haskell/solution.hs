strints :: String -> [Int]
strints s = map read (words s)

nums x = iterate (const x) x

build s e l v = take (s-1) (nums 0) ++ take (e-s+1) (nums v) ++ take (l-e) (nums 0)

solve2 :: Int -> [[Int]] -> [Int] -> Int
solve2 _     []    acc = foldl max 0 acc
solve2 opcol (h:t) acc = solve2 opcol t (zipWith (+) -- optimise me
                                         (build (h!!0) (h!!1) opcol (h!!2))
                                         acc)

solve :: [Int] -> [[Int]] -> Int
solve [opcols, _] queries = solve2 opcols queries zeros
  where zeros = replicate opcols 0

main :: IO ()
main = do
  cont <- getContents
  let ls = lines cont
  let ia = strints     $ head ls
  let dt = map strints $ tail ls

  print (solve ia dt)
