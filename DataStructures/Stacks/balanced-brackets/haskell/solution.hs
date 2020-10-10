-- solution

module Main where

import Control.Monad
import System.Environment
import System.IO

cb :: String
cb = ")}]"

mb :: Char -> String -> Bool
mb c stack =
  (c == ')' && '(' == head stack) ||
  (c == '}' && '{' == head stack) ||
  (c == ']' && '[' == head stack)

isEmptyStack :: String -> Bool
isEmptyStack stack = words stack == []

balanced :: String -> String -> Bool
balanced str stack =
  if str == ""
  then isEmptyStack stack
  else
    balanced (tail str) ns
    where
      fc = head str
      c = fc == ')' ||  fc == '}' || fc == ']'
      f = c && mb fc stack
      ns = if f then tail stack else fc : stack

-- Complete the isBalanced function below.
isBalanced s =
  if balanced s [] then "YES" else "NO"

main :: IO()
main = do
    stdout1 <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout1 WriteMode

    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        s <- getLine

        let result = isBalanced s

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
