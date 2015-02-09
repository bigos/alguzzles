import Control.Monad
import Data.List
import Network.BSD

main :: IO ()
main = do
  hostName <- getHostName
  if isInfixOf "acek" hostName
    then do
    print "zzz"
    fh <- open "input1.txt" ReadMode
    n <- readLn fh :: IO Int
    str <- replicateM n (getLine fh)
    else do
    n <- readLn :: IO Int
    str <- replicateM n getLine

  let
    ans = map (sum. map read. words) str
          mapM_ print ans
