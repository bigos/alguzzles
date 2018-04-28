solveMe aa bb ac bc = if null aa
  then ((show ac) ++ " " ++ (show bc))
  else solveMe (tail aa) (tail bb) na nb
  where a = head aa
        b = head bb
        na = (if a>b then succ ac else ac)
        nb = (if a<b then succ bc else bc)

splitAndParse :: String -> [Integer]
splitAndParse s = map read (words s)

main = do
  line1 <- getLine
  line2 <- getLine
  putStrLn (solveMe (splitAndParse line1) (splitAndParse line2) 0 0)
