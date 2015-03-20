module Main where

import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B

maxSums :: [Int] -> (Int, Int)
maxSums (n:ns) = maxSums' ns n n n
 where
  maxSums' []     _  mc mn = (mc, mn)
  maxSums' (n:ns) mh mc mn = let mh' = maximum [n, mh + n]
                                 mc' = maximum [mh', mc]
                                 mn' = maximum [n, mn + n, mn]
                             in maxSums' ns mh' mc' mn'

solve :: [B.ByteString] -> String
solve (_:xs) = solve' xs []
 where
  solve' []        os = unlines os
  solve' (_:ns:xs) os = let o = maxSums. map fst. catMaybes. map B.readInt. B.words$ ns
                        in solve' xs (os ++ [show (fst o) ++ " " ++ show (snd o)])

main :: IO ()
main = B.getContents >>= putStr. solve. B.lines
