-- password cracker

import Control.Monad (replicateM)

comb :: Int -> [a] -> [[a]]
comb m xs = combsBySize xs !! m
 where
   combsBySize = foldr f ([[]] : repeat [])
   f x next = zipWith (++) (map (map (x:)) ([]:next)) next

-- try it with
-- solve 3 ["ab", "abcd", "cd"] "abcd"
-- I need to find a recursive way of solving the problem
solve n ns la = replicateM n ns

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
  mapM_ (\_ -> (readTestCase >>= (\(n,ns,la) -> putStrLn (show (solve n ns la)))))
    [1..t]
