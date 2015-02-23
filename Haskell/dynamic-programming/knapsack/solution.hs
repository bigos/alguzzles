import Control.Monad
import Data.List

getData :: IO  (Int, Int, [Int])
getData = do
  dd <- fmap (map read . words) getLine
  let n = head dd
  let k = last dd
  ar <- fmap (map read . words) getLine
  return (n, k, ar)

main :: IO ()
main = do
  t <- readLn :: IO Int
  -- print t
  inputs <- replicateM t getData
  print inputs
  --mapM_ (\x -> putStrLn (showmaxes (snd x))) inputs
