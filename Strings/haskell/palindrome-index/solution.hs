import Data.List
import Data.Array

main :: IO ()
main = do
  mydata <- getContents
  let mylines = lines mydata
  let tc = read (head mylines) ::Int
  print (tc ,   (tail mylines ))

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
