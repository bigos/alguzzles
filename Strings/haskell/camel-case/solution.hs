import Data.Char

solution :: String -> Int
solution s = upcases + 1
  where upcases = length $ filter (\x -> x == True)  [ isUpper x | x <- s ]

main :: IO()
main = do
  s0 <- getLine
  let s = s0
  print $ solution s
