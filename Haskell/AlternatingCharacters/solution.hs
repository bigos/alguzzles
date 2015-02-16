import Control.Monad
-- import Debug.Trace
                --
res :: [Char] -> Int -> Int
res (x:y:ys) acc
  -- | trace ("in res " ++ show (x:y:[]) ++ " - " ++ show ys ++ " " ++ show acc) False = undefined
  | ys == [] = if x== y then (acc+1) else acc
  | x == y = res (y:ys) (acc + 1)
  | otherwise = res (y:ys) (acc + 0)

solve :: [String] -> [Int]
solve strs = map (\x -> res x 0) strs

main :: IO ()
main = do
  t <- readLn :: IO Int
  strs <- replicateM t getLine
  let solutions = solve strs
  mapM_ print solutions
