-- import Debug.Trace
import Prelude
-- import Control.Monad

numbers = "0123456789"
lower_case = "abcdefghijklmnopqrstuvwxyz"
upper_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
special_characters = "!@#$%^&*()-+"

member c s =  not (null found)
  where found = filter ( == c) s

solve l =
  if (pl < 6)
  then 6 - cs
  else 4 - cs
  where
    c = read (head l) :: Integer
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
  getContents >>=  putStrLn . show . solve . lines
