import Control.Monad
import System.IO

main :: IO ()
main = do
  if 1 == 1
    then do
      let stdin = openFile "input1.txt" ReadMode
      print "---"
      return ()
    else do
      return ()

  n <- readLn :: IO Int
  str <- replicateM n getLine
  let
    ans = map (sum. map read. words) str
  mapM_ print ans
