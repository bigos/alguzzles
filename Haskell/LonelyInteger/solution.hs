import Data.List
import Debug.Trace
-- very good writeup
-- http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html

isTheSame :: (Int, Int) -> Bool
isTheSame (x, y) = (x == y)

splitList :: (Int, [a]) -> ([a], [a])
splitList (ln, lst) = splitAt (div ln 2) lst

sortLists :: Ord a => ([a], [a]) -> ([a], [a])
sortLists sl =
  let l1 = Data.List.sort (fst sl)
      l2 = Data.List.sort (snd sl)
  in (l1, l2)

get2Lines :: IO (String, String)
get2Lines = do
  print "enter line 1"
  line1 <- getLine
  print "enter line 2"
  line2 <- getLine
  return (line1, line2)


findDifferent :: ([Int], [Int]) -> Int
findDifferent (x, y) | trace ("+++++ " ++ show x ++ " " ++ show y) False = undefined
findDifferent ([x], []) = x
findDifferent ([], [y]) = y
findDifferent sorted =
  let fh = head (fst sorted)
      sh = head (snd sorted)
  in (if fh == sh
      then findDifferent ((drop 1 (fst sorted)),(drop 1 (snd sorted)) )
      else fh)


main :: IO ()
main = do
  (val1, val2) <- get2Lines
  let va = (read val1 :: Int)
  let vb = (map read (words val2) :: [Int])
  let vr = (isTheSame (va, head vb) )
  let sl = splitList (va, vb)
  let sorted = sortLists sl
  print sorted
  let foundDifferent = findDifferent sorted
  print foundDifferent
  print "above is the result"
  print vr
  print va
  print vb
