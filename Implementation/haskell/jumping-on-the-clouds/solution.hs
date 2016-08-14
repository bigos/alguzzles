import Data.Char

split1 :: [Int] -> [[Int]] -> [Int] -> [[Int]]
split1 [] a c = a ++ [c]
split1 l  a c = if head l == 1 then x else y
  where x = split1 (tail l) (a ++ [c]) []
        y = split1 (tail l) a (c ++ [head l])

split :: [Int] -> [[Int]]
split l = split1 l [] []

sublenx :: [Int] -> Int
sublenx a = div (length a) 2

solution s = sum sublens + divlen
  where subs = split s
        divlen = (length subs) - 1
        sublens = map (\x -> sublenx x ) subs

main :: IO()
main = do
  n0 <- getLine
  s0 <- getLine
  let n = read n0 :: Int
  let s = map read $ words s0 :: [Int]
  print $ solution s
