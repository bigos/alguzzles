import Control.Monad
import Data.List

splitInt :: [[Int]] -> [Int] -> [Int] -> [[Int]]
splitInt [] m n = [m, n]
splitInt aa m n = splitInt (tail aa) mx nx
  where h0 = (head aa)!!0
        fe = (head aa)!!1
        mx = if fe == 0 then (h0:m) else m
        nx = if fe == 1 then (h0:n) else n

solution :: Int -> [[Int]] -> Int
solution k aa = lis + mis
  where mySplit = splitInt aa [] []
        lessImp = sort $ head mySplit
        moreImp = reverse $ sort $ head $ tail mySplit
        lis = sum lessImp
        mis = (sum (take k moreImp)) - (sum (drop k moreImp))

line2int :: String -> [Int]
line2int s = map read $ words s

main :: IO()
main = do
  countLine <- getLine
  let n = head $ map read $ words countLine
      k = head $ tail $ map read $ words countLine :: Int
  otherLines0 <- replicateM n getLine
  let otherLines = map line2int otherLines0
  print (solution k otherLines)
