import Debug.Trace


main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let ar = (map read (words arstr)) :: [Int]
  print s
  print ar
