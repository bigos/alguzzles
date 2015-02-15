import Control.Monad

-- solution suggestion, 4 removed
-- a-a-a+b-b-b+(end)
-- - -   - -

recme s e acc
  | s >= e = acc
  | otherwise = recme (s + 1) e s

res (x:y:ys) acc
  | ys == [] = acc
  |


solveOne :: String -> Int
solveOne "A" = 0
solveOne "B" = 0
-- solveOne "AA" = 1
-- solveOne "BB" = 1
solveOne s = 1

solve :: [String] -> [Int]
solve strs = map solveOne strs

main :: IO ()
main = do
  t <- readLn :: IO Int
  strs <- replicateM t getLine
  let solutions = solve strs
  mapM_ print solutions
