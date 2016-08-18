import Control.Monad

line2int :: String -> [Int]
line2int s = map read $ words s

main :: IO()
main = do
  countLine <- getLine
  let n = head $ map read $ words countLine
      k = head $ tail $ map read $ words countLine :: Int
  otherLines0 <- replicateM n getLine
  let otherLines = map line2int otherLines0
  print (n,k,otherLines)
