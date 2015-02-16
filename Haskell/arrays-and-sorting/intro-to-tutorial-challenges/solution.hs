import System.IO

main :: IO ()
main = do
  v <- readLn :: IO Int
  n <- readLn :: IO Int
  print "2 vars"
  arstr <- getLine
  print arstr
  let ar = map (read $ words ) arstr :: [Int]
  print v
  print n
  print ar
