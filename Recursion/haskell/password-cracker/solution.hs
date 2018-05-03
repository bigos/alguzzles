-- password cracker

import Data.List

guess :: [String] -> String -> [String] -> [String]
guess keys "" acc = acc
guess keys str acc = map (\k -> if (isPrefixOf k str)
                           then (guess keys (drop (length k) str) (k:acc))
                           else [])
                     keys


solve :: [String] -> [String] -> String -> [String] -> [String]
solve _ _  "" acc = reverse acc
solve _ [] _  _   = []
solve ngo (n:ns) la acc  =
  if (isPrefixOf n la)
  then solve ngo ngo (drop (length n) la) (n:acc)
  else solve ngo ns la acc

flt a = (filter (\x-> x /= []) a)

runner []     la a = if (null (flt a)) then ["WRONG","PASSWORD"] else  (head (flt a))
runner (n:ns) la a = runner ns la ((solve (n:ns) (n:ns) la []) : a)

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
  mapM_ (\_ -> (readTestCase >>= (\(_, ns, la) -> putStrLn (unwords (runner  ns la []))))) [1..t]
