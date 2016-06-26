import Prelude
import Data.Map (toList, fromListWith)

palindromic input = length odds < 2
  where
    odds = filter odd (map snd (strFreq input))

strFreq :: String -> [(Char, Int)]
strFreq input = toList $ fromListWith (+) [(c, 1) | c <- input]

main :: IO ()
main = do
  mydata <- getContents
  let mylines = lines mydata
  let tc = read (head mylines) ::Int
  print (tc ,   (tail mylines ))

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
