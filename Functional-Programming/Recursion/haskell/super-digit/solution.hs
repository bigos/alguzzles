import Control.Monad
-- import Debug.Trace

       -- not working

getData = do
  dd <- fmap (map read . words) getLine
  let n = head dd :: Int
  let k = last dd :: Int
  return (n, k)

main :: IO ()
main = do
  nk <- getData
  print  nk
