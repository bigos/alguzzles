import Data.List
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

get2Lines = do
  print "enter line 1"
  line1 <- getLine
  print "enter line 2"
  line2 <- getLine
  return (line1, line2)

main = do
  (val1, val2) <- get2Lines
  let va = (read val1 :: Int)
  let vb = (map read (words val2) :: [Int])
  let vr = (isTheSame (va, head vb) )
  let sl = splitList (va, vb)
  let sorted = sortLists sl
  print sorted
  print vr
  print va
  print vb
