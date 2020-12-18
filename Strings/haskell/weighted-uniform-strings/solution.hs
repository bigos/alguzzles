module Main where
-- https://www.hackerrank.com/challenges/weighted-uniform-string/problem

import Data.List
import Data.Maybe
import Debug.Trace

theInput = "abccddde\n6\n1\n3\n12\n5\n9\n10\n"

huh1 = groupSame [1,2,3] 0 [] []
huh2 = groupSame [1,2,2,2,3] 0 [] []
huh3 = groupSame [1,2,2,3,3,3,4,4,5,4,4,4,3,3,2,2,2,2] 0 [] []

groupSame seqn lastel sames acc = --trace (show (seqn,lastel,sames," acc ",acc)) $
  if null seqn
  then init endacc2
  else (if lastel == head seqn
        then groupSame nv (head seqn) (head seqn : sames) acc
        else groupSame nv (head seqn) [] endacc2)
  where
    nv = tail seqn
    endacc2 = if null sames
      then [lastel] : acc
      else scanl (+) lastel  sames : acc

chval :: Char -> Integer
chval ch = res
  where
    lu = find (\(k,_) -> k == ch) (zip ['a'..'z'] [1..])
    res = case lu of
      Nothing -> -1
      Just n -> snd n

solution s qn dd = --trace (show (s, qn, dd)) $
  unlines $ yesno results
  where
    ans = map chval s
    rns = concat $ groupSame ans 0 [] []
    gg = map maybebool $ map (\n -> find ( == n) rns) dd
    --aa = trace (show ("AAAAAAAAAAAAAa", gg)) $ 1
    results = gg

maybebool v = case v of
  Nothing -> False
  Just n -> True

yesno bb =  map (\b -> if b then "Yes" else "No") bb

tri1 (a, _, _) = a
tri2 (_, a, _) = a
tri3 (_, _, a) = a

mydata :: String -> (String, Int, [Integer])
mydata ll = (s, cnt, dat)
  where (s : c : d) = lines ll
        cnt = read c
        dat = map read d

experiment = solution s q d
  where a = mydata theInput
        s = tri1 a
        q = tri2 a
        d = tri3 a

main :: IO ()
main = do
  cont <- getContents
  let d  = mydata cont
  let s  = tri1 d
  let qn = tri2 d
  let dd = tri3 d
  putStrLn $ solution s qn dd

-- interesting solutions
-- https://www.hackerrank.com/rest/contests/master/challenges/weighted-uniform-string/hackers/kadoban/download_solution
-- https://www.hackerrank.com/rest/contests/master/challenges/weighted-uniform-string/hackers/marcor/download_solution?primary=true
-- import Control.Applicative
-- import Control.Monad
-- import System.IO
-- import Data.Char (ord)
-- import Data.List
-- import qualified Data.IntSet as IntSet

-- weigthUniformSubstrings [] = IntSet.empty
-- weigthUniformSubstrings s = IntSet.fromList $ do
--     s'@(c:_) <- group s
--     let n = length s'
--     let w = weight c
--     i <- [1..n]
--     return $ i * w

-- weight :: Char -> Int
-- weight c = (ord c) - (ord 'a') + 1

-- main :: IO ()
-- main = do
--     s <- getLine
--     n_temp <- getLine
--     let n = read n_temp :: Int
--     let u = weigthUniformSubstrings s
--     forM_ [1..n] $ \a0  -> do
--         x_temp <- getLine
--         let x = read x_temp :: Int
--         putStrLn $ if x `IntSet.member` u then "Yes" else "No"

-- https://www.hackerrank.com/rest/contests/master/challenges/weighted-uniform-string/hackers/_Jie_/download_solution
-- import qualified Data.Set as S
-- import Data.Char
-- import Data.List

-- main = do
--   word <- getLine
--   getLine
--   q <- map read . lines <$> getContents
--   let s = subStrSums word
--   mapM_ (putStrLn . (\n -> if S.member n s then "Yes" else "No" )) q

-- subStrSums :: String -> S.Set Int
-- subStrSums = S.fromList . concatMap (scanl1 (+)) . group . map (\c -> ord c - ord 'a' +1)

-- https://www.hackerrank.com/rest/contests/master/challenges/weighted-uniform-string/hackers/hurryabit/download_solution
-- import Data.Char
-- import Data.List
-- import qualified Data.Set as Set
-- weights = Set.fromList . concatMap f . group where
--   f cs@(c:_) = let k = ord c - ord 'a' + 1
--                in  [ k*i | i <- [1..length cs] ]
-- main = do
--   cs:_:qs <- lines <$> getContents
--   let w = weights cs
--   mapM_ (\q -> putStrLn $ if read q `Set.member` w then "Yes" else "No") qs
