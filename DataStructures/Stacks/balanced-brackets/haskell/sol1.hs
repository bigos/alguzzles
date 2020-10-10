-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Applicative

balParen :: String -> String
balParen s = help [] s
    where help [] [] = "YES"
          help [] (y:ys) = help [y] ys
          help (x:xs) [] = "NO"
          help (x:xs) (y:ys) =
              case y of
                  ']' -> if x == '[' then help xs ys
                         else "NO"
                  ')' -> if x == '(' then help xs ys
                         else "NO"
                  '}' -> if x == '{' then help xs ys
                         else "NO"
                  _   -> help (y:x:xs) ys
main = do
    t <- getLine
    tests <- lines <$> getContents
    mapM_ (putStrLn . balParen) tests
