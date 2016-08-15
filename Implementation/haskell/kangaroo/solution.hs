import Data.List

convergent :: [Int] -> [Int] -> [Int]
convergent a b = intersect a b

positions s j = take 10 [s,(s+j)..]

solution :: [Int] -> String
solution l = if convergent a b == [] then "NO" else checkSteps
  where a = positions (l!!0) (l!!1)
        b = positions (l!!2) (l!!3)
        zzz = filter (\z -> z == True) $ map (\x -> elemIndex x a == elemIndex x b ) (convergent a b)
        checkSteps = if zzz /= [] then "YES" else "NO"

main :: IO()
main = do
  l0 <- getLine
  let l = map read (words l0) :: [Int]
  putStr $ solution l
