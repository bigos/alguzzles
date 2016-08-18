import Control.Monad

solution :: [[Int]] -> [Int]
solution a = [1]                -- TODO:

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
