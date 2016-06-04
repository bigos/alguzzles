import Data.List
import Data.Function
import Control.Monad
import Text.Printf

doit :: Int -> [Int] -> [Int] -> Int -> Int -> [Int]
doit _ [] acc _ _ = acc
doit k probs acc page sol
  | endChapter = doit k (tail probs) acc1 nextpage 1
  | otherwise = doit k probs acc2 nextpage nextsol
  where
    nextpage = page + 1
    maxsol = head probs
    solstep = sol + k
    nextsol = min maxsol solstep
    endChapter = solstep > maxsol
    specials x = page == x
    acc1 = (filter specials [sol .. (nextsol)]) ++ acc
    acc2 = (filter specials [sol .. (nextsol-1)]) ++ acc


splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

strf :: Int -> Int -> String
strf a b = printf "%d %d" a b

main :: IO()
main = do
  l1 <- getLine
  l2 <- getLine
  let k = last $ splitAndRead l1
  let tx = splitAndRead l2
  print (length (doit k tx [] 1 1))
