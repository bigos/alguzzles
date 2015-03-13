module Main
( main
) where

import           Prelude hiding (interact, lines, unlines, words)
import           Data.Text (pack, unpack, Text, lines, unlines, words)
import           Data.Text.IO (interact)
import           Data.List (foldl')

type N = Int

maxSubArray :: [N] -> (N, N)
maxSubArray = foldl' processOne (sentinel, sentinel, sentinel) & coerceOutput
    where coerceOutput (_, a, b) = (a, b)
          sentinel = minBound `div` 3 -- div3 to avoid underflow on addition
                                      -- of negative

processOne :: (N, N, N) -> N -> (N, N, N)
processOne (maxHere, maxMax, posMax) a = (maxHere', maxMax', posMax')
    where maxHere' = max a      (a + maxHere)
          maxMax'  = max maxMax maxHere'
          posMax' | posMax < 0 = max posMax a
                  | a < 0      = posMax
                  | otherwise  = posMax + a

handleInput :: Text -> Text
handleInput = lines & drop 2 & everyOther & map handleLine -- now [[N]]
            & map maxSubArray                              -- now [(N, N)]
            & map (showAns & pack) & unlines               -- now Text
    where handleLine = words & map toInt
          toInt :: Text -> N
          toInt = unpack & read
          showAns (a, b) = show a ++ ' ' : show b

everyOther :: [a] -> [a]
everyOther (a:_:xs) = a : everyOther xs
everyOther xxs@[_]  = xxs
everyOther _        = []

main :: IO ()
main = interact handleInput

(&) :: (a -> b) -> (b -> c) -> a -> c
(&) = flip (.)
infixl 9 &
