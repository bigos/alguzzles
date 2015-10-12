module Main where

import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as M
import qualified Data.IntSet as S

quickest :: M.IntMap Int -> Int
quickest m = quickest' (S.singleton 1) 0
 where
  quickest' s n | S.member 100 s = n
                | otherwise      = quickest' (S.foldl' moves S.empty s) (n + 1)
  moves s k = S.union s (S.fromList [M.findWithDefault k' k' m | k' <- [k + 1 .. min 100 (k + 6)]])

solve :: [B.ByteString] -> [Int]
solve ss = solve' ss []
 where
  solve' []         os = os
  solve' (_:l:s:ss) os = solve' ss (os ++ [quickest (board l s)])
  board l s = M.fromList (map pair ((B.words l) ++ (B.words s)))
  pair s = let p = map (fst. fromJust. B.readInt) (B.split ',' s)
           in (p !! 0, p !! 1)

main :: IO ()
main = mapM_ print. solve. tail. B.lines =<< B.getContents
