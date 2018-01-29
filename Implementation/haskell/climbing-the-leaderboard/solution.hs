-- climbing the leaderboard

rank :: Int -> [Int] -> Int -> Int
rank r [] a = a
rank r (s:ss) a =
  if (r >= s)
  then a
  else rank r ss (if ss/= [] && head ss == s
                  then a
                  else (succ a))

solve :: t1 -> [Int] -> t -> [Int] -> [Int]
solve n ss m as = map (\x -> rank x ss 1) as

readNumber :: IO Int
readNumber = getLine >>= (\n -> return (read n))

readInts :: IO [Int]
readInts = getLine >>= (\ln -> return (map (\x -> read x) (words ln)))

--main :: IO ()
main =
  readNumber >>=
  (\n -> readInts >>=
  (\ss -> readNumber >>=
  (\m -> readInts >>=
  (\as -> mapM_ (\z-> print z) (solve n ss m as)))))
