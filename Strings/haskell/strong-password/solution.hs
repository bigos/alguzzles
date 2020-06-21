-- import Debug.Trace
-- import Prelude
-- import Control.Monad

numbers :: String
numbers = "0123456789"

lower_case :: String
lower_case = "abcdefghijklmnopqrstuvwxyz"

upper_case :: String
upper_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

special_characters :: String
special_characters = "!@#$%^&*()-+"

member :: Char -> String -> Bool
member c s = c `elem` s

solve :: [String] -> Int
solve l =
  if pl < 6
  then 6 - cs
  else 4 - cs
  where
    -- ignore head l
    p = last l
    lf fn = length $ filter (`member` fn) p
    pl = length p
    nx = min 1 $ lf numbers
    lc = min 1 $ lf lower_case
    uc = min 1 $ lf upper_case
    sc = min 1 $ lf special_characters
    cs = sum [nx, lc, uc, sc]

main :: IO ()
main =
  getContents >>=  print . solve . lines
