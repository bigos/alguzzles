import Control.Monad

-- solution suggestion, 4 removed
-- a-a-a+b-b-b+
-- - -   - -

solve :: [String] -> [Int]
solve strs = [1,2,3] -- finish me

main :: IO ()
main = do
  t <- readLn :: IO Int
  strs <- replicateM t getLine
  let solutions = solve strs
  mapM_ print solutions
