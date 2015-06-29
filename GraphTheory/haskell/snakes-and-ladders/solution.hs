import Control.Monad
import Data.List

getCases = do
  ladders <- readLn :: IO Int
  ladderData <- replicateM ladders getLine
  snakes <- readLn :: IO Int
  snakeData <- replicateM snakes getline
  return (ladders, ladderData, snakes, snakeData)

main :: IO ()
main = do
  t <- readLn :: IO Int
  cases <- replicateM t getCases
  print cases
