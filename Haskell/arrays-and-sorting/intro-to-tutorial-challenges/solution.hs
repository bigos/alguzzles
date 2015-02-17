import System.IO

main :: IO ()
main = do
  v <- readLn :: IO Int
  n <- readLn :: IO Int
  arstr <- getLine
  let arl = words arstr
  let ar = map read arl :: [Int]
  print v
  print n
  print ar
