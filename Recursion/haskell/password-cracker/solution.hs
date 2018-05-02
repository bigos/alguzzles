-- password cracker

import Data.List

solve :: [String] -> [String] -> String -> [String] -> [String]
solve _ _  "" acc = reverse acc
solve _ [] _  _   = ["WRONG","PASSWORD"]
solve ngo (n:ns) la acc  =
  if (isPrefixOf n la)
  then solve ngo ngo (drop (length n) la) (n:acc)
  else solve ngo ns la acc

testCases :: IO Integer
testCases = getLine >>= (\x -> return (read x))

readTestCase :: IO (Int, [String], String)
readTestCase = do
  n <- getLine
  ns <- getLine
  la <- getLine
  return ((read n), words ns, la)

main :: IO ()
main = do
  t <- testCases
  mapM_ (\_ -> (readTestCase >>= (\(_, ns, la) -> putStrLn  (unwords(solve ns ns la []))))) [1..t]
