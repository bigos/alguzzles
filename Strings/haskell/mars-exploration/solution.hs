import Data.Char

sol :: String -> Int -> Int
sol "" c = c
sol s  c = sol (drop 3 s) (nc + c)
  where ltrs = take 3 s
        lt0 = ltrs !! 0 == 'S'
        lt1 = ltrs !! 1 == 'O'
        lt2 = ltrs !! 2 == 'S'
        nc = length $ filter (\x -> x == False) [lt0, lt1, lt2]

solution s = sol s 0

main :: IO()
main = do
  s0 <- getLine
  let s = s0
  print $ solution s
