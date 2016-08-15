import Data.List

positions s j = take 10000 [s,(s+j)..]

solution :: [Int] -> String
solution l = if intersect a b == [] then "NO" else checkSteps
  where a = positions (l!!0) (l!!1)
        b = positions (l!!2) (l!!3)
        zzz = filter (\z -> z == True) $ map (\x -> elemIndex x a == elemIndex x b) (intersect a b)
        checkSteps = if zzz /= [] then "YES" else "NO"

main :: IO()
main = do
  l0 <- getLine
  let l = map read (words l0) :: [Int]
  putStr $ solution l
