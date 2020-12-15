module Main where
-- https://www.hackerrank.com/challenges/weighted-uniform-string/problem



theInput = "abccddde\n6\n1\n3\n12\n5\n9\n10\n"

solution = [True, True, False]
yesno bb =  map (\b -> if b then "Yes" else "No") bb



mydata :: String -> ((String, Int), [Int])
mydata ll = ((s, cnt), dat)
  where
    (s : c : d) = lines ll
    cnt = read c
    dat = map read d

main :: IO ()
main = do
  cont <- getContents
  let d = mydata cont

  let s = fst  $ fst d
  let qn = (snd (fst d))
  let dd = (snd d)

  putStrLn $ show (s, qn, dd)
