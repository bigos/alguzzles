-- password cracker

import Data.List

--guess :: [String] -> String -> [String]
guess []     "" acc = reverse acc
guess []     la acc = ["WRONG", "PASSWORD"]
guess _      "" acc = reverse acc
guess (n:ns) la acc =
  if (isPrefixOf n la)
  then guess ns (drop (length n) la) (n : acc)
  else guess ns la acc

guess2 :: [String] -> String -> String
guess2 ns la = unwords (guess ns la [])

testCases :: IO Integer
testCases = getLine >>= (\x -> return (read x))

readTestCase :: IO (Int, [String], String)
readTestCase = do
  n <- getLine
  ns <- getLine
  la <- getLine
  return ((read n), words ns, la)

main = do
  t <- testCases
  mapM_ (\_ -> (readTestCase >>= (\(_, ns, la) -> putStrLn (guess2 ns la)))) [1..t]
