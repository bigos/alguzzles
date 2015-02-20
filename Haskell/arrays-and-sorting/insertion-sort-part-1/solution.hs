import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace

-- 2468 (3) replace 3 with nothing if 3 < 8
-- 2468 ()
-- 2468 (8) 2468 ++ last of 2468 ++ () ;  sliced 3 [2,4,6,8]
-- 246 (68) 246 ++ last of 246 ++ (8)
-- 24 (468) 24 ++ last of 24 ++ (68)
-- 2 (2468) replace 2 with 3 because 3 >= 2 ; 2 ++ last of 2 ++ (468)
-- 2 (3468) DONE

mybeg i ar = take (i+1) ar
myfin i ar = drop i ar

sliced i ar = (mybeg i ar, myfin i ar)

disp done sl v = join [fst sl, if done then ([v]++ tail (snd sl)) else snd sl]

-- insort 3 3 [2,4,6,8] []
insort i v ar res
  | trace (show i ++ show v ++ show ar++ show sl ) False = undefined
  | trace (show (disp done sl v)) False = undefined
  | done = res ++ [(disp done sl v)]
  | i < 0 =  res ++ [[v]++(disp done sl v)]
  | otherwise = insort (i-1) v ar (res++ [ (disp done sl v)])
  where sl = sliced i ar
        head2nd = (head $ snd sl)
        done = if head2nd <= v  then True else False

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight str | all isSpace str = ""
trimRight (c : cs) = c : trimRight cs

trim :: String -> String
trim = trimLeft . trimRight

ar2str ar = trim (concatMap (\x -> (show x)++" " ) ar)

main :: IO ()
main = do
  s <- readLn :: IO Int
  arstr <- getLine
  let arwords = words arstr
  let ar = (map read arwords) :: [Int]
  -- print s
  -- print ar
  let ans = insort ((length ar)-2) (last ar) (init ar) []
  mapM_ (\x -> putStrLn (ar2str x)) ans
