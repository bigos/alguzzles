-- password cracker

import Data.List
-- import Debug.Trace

solve ns "" = ""
solve ns la =
  if findpass
  then ""
  else la
  where findpass = (any (\x->  x == "") (map (\s -> if (isPrefixOf s la)
                                               then (solve ns (drop (length s) la) )
                                               else la )
                                          ns))

solve2 _ ns la = if (solve ns la == "") then la else "WRONG PASSWORD"

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
  mapM_ (\_ -> (readTestCase >>= (\(n,ns,la) -> putStrLn (solve2 n ns la )))) [1..t]
