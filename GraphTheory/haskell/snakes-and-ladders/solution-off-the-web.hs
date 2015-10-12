module Main where
import Data.List
import Data.Maybe
import Data.Function
import Control.Monad

str2int s = read s :: Int
str2ints s = read ('[' : s ++ "]") :: [Int]
str2pairs = map str2ints . words
a2p a = let [x, y] = a in (x, y)

reduce f l = (fst $ head l, foldl1 f $ map snd l)

search a h = [y | (x, y) <- h, x == a]

sure (Just x) = x

sp g x y = sure . lookup y $ head $ filter (elem y . map fst) $ iterate f [(x, 0)]
  where f = floodfill g

solve e = sp g 1 100
  where g = concat [ [(x, y) | y <- f x ] | x <- [1 .. 99] ]
          where f x = let t = lookup x e
                      in  case t of
                        Just y -> [y + d | d <- [1..6], y + d <= 100 ]
                        _ -> [x + d | d <- [1..6], x + d <= 100]

main = getLine >>= (flip replicateM_ work) . str2int where
  work = getLine >>= subwork . str2ints where
    subwork [m, n] = replicateM 2 (getLine >>= return . str2pairs) >>=
                     print . solve . (map a2p) . concat

floodfill g s = map (reduce min) $ groupBy ((==) `on` fst) $ sort $
                s ++ concat [ [ (y, d + 1) | y <- search x g ] | (x, d) <- s ]
