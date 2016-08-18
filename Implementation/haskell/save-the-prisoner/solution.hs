import Control.Monad

zzz :: [Int] -> Int
zzz a = 1 + mod (x - 1) n
  where n = a!!0
        m = a!!1
        s = (a!!2)-1
        x = mod (s + m) n

solution :: [[Int]] -> [Int]
solution aa = map zzz aa

line2int :: String -> [Int]
line2int s = map read $ words s

main :: IO()
main = do
  t0 <- getLine
  let t =  read t0 :: Int
  lines0 <- replicateM t getLine -- read t lines
  let lines1 = map line2int lines0
      results = solution lines1
  mapM_ (\x -> print x) results
