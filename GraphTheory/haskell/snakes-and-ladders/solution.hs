import Control.Monad
import Data.List

getCaseData = do
  count <- readLn :: IO Int
  theData <- replicateM count getLine
  return (count, theData)

getCases = do
  ladders <- getCaseData
  snakes <- getCaseData
  return (fst ladders, snd ladders, fst snakes, snd snakes)

main :: IO ()
main = do
  t <- readLn :: IO Int
  cases <- replicateM t getCases
  print cases
