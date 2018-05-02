-- password cracker

import Data.List

guess _      "" a = reverse a
guess []     la a = []
guess (n:ns) la a = if (isPrefixOf n la)
  then (guess (n:ns) (drop (length n) la) (n:a))
  else (guess ns la a)

runner2 [] la a = a
runner2 (n:ns) la a = runner2 ns la ((guess (n:ns) la []) : a)

runner ns la a = if (null filz) then ["WRONG","PASSWORD"] else head filz
  where
    dat = runner2 ns la a
    filz =  filter (\x-> x/=[]) dat


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
  mapM_ (\_ -> (readTestCase >>= (\(_, ns, la) -> putStrLn  (unwords(runner ns la []))))) [1..t]
