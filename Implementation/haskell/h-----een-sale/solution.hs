-- solution

import System.IO

solve2 :: Int -> Int -> Int -> Int  -> Int -> Int
solve2    p      d      m      s       acc = 1 -- finish me

solve :: [Int] -> Int
solve (p:d:m:s:[]) = solve2 p d m s 0

main :: IO ()
main = do
  let fromStdin = False --True or False
  handle <- if fromStdin then pure stdin else openFile "./input0.txt" ReadMode
  ln <- hGetLine handle
  let nums = (map (\x -> read x::Int) (words ln))
  print (solve nums)
  hClose handle
