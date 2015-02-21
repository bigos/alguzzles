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

-- insort 3 3 [2,4,6,8] []
insort i v ar res
  | trace (show i ++ show v ++ show ar++ show sl ) False = undefined
  | trace (show (disp done sl v)) False = undefined
  | done = res ++ [(disp done sl v)]
  | i < 0 =  res ++ [[v]++(disp done sl v)]
  | otherwise = insort (i-1) v ar res
  where sl = sliced i ar
        head2nd = (head $ snd sl)
        done = if head2nd <= v  then True else False

main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let ar = (map read (words arstr)) :: [Int]
  print s
  print ar
