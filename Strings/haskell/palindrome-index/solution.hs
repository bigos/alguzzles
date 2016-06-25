import Data.List
import Data.Array
import System.IO

main :: IO ()
main = do
  args <- getContents
  -- inputArgs <- getData
  --process inputArgs
  print (lines args)

-- do notaion explained
-- https://en.wikibooks.org/wiki/Haskell/do_notation
