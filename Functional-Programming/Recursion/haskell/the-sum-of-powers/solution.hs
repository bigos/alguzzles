-- solution

solve :: Integer -> Integer -> Integer
solve x n = 1

main = do
  --contents <- getContents
  let contents = "10\n2"

  let x = read ((lines contents)!!0)::Integer
  let n = read ((lines contents)!!1)::Integer
  print  (show (solve x n))
