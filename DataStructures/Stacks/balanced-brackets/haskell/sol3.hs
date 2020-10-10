-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Data.Functor
import Control.Monad

main = do
  t <- read <$> getLine
  replicateM_ t oneCase

oneCase = do
  input <- getLine
  if valid input then
    putStrLn "YES"
  else
    putStrLn "NO"

valid = go [] where
  go [] [] = True
  go ss (a:as)
    | a == '(' = push
    | a == '[' = push
    | a == '{' = push
    where
      push = go (a:ss) as
  go (s:ss) (a:as)
    | s == '(' && a == ')' = pop
    | s == '[' && a == ']' = pop
    | s == '{' && a == '}' = pop
    where
      pop = go ss as
  go _ _ = False
