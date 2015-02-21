import Control.Monad
import Debug.Trace
import Data.List

mybeg i ar = take (i+1) ar
myfin i ar = drop (i+1) ar

sliced i ar = (mybeg i ar, myfin i ar)

disp done sl v = join [fst sl, if done then ([v]++ tail (snd sl)) else snd sl]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted  = quicksort [a | a <- xs, a >  x]
  in  smallerSorted ++ [x] ++ biggerSorted

-- insort 0 [2,4,6,8] []
insort i ar res
  -- | trace (show i ++ show ar++ show sl ++">>> " ++show res) False = undefined
  | i == length ar = res
  | otherwise = insort (i+1) ar (res ++  [join [(quicksort (fst sl)), snd sl] ])
  where sl = sliced i ar
        range = [1..(length ar - 1)]

main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let ar = (map read (words arstr)) :: [Int]
  print s
  print ar
