import Data.List
import Data.Function
import Control.Monad


str2ints :: String -> [Int]
str2ints s = map (\x -> read x) (words s)

tuplify :: [String] -> [(Int, Int)]
tuplify zz = map (\x -> (x !! 0, x !! 1)) (map str2ints zz)

findminx :: [(Int, Int)] -> Int
findminx a = minimum $ map fst a

findminy :: [(Int, Int)] -> Int
findminy a = minimum $ map snd a

findmaxx :: [(Int, Int)] -> Int
findmaxx a = maximum $ map fst a

findmaxy :: [(Int, Int)] -> Int
findmaxy a = maximum $ map snd a

findBoundaries :: [(Int, Int)] -> (Int, Int, Int, Int)
findBoundaries a = (findminx a,
                   findmaxx a,
                   findminy a,
                   findmaxy a)

averx :: (Int, Int, Int, Int) -> Int
averx (l, h, _, _) = div (l + h) 2

avery :: (Int, Int, Int, Int) -> Int
avery (_, _, l, h) = div (l + h) 2

midpoints :: (Int, Int, Int, Int) -> (Int, Int)
midpoints cc = (averx cc, avery cc)

sortPoints [] midpoints tr tl bl br =
  [sortBy (\ x y -> compare (fst x) (fst y) ) tr,
   sortBy (\ x y -> compare (fst x) (fst y) ) tl,
   reverse $ sortBy (\ x y -> compare (fst x) (fst y) ) bl,
   reverse $ sortBy (\ x y -> compare (fst x) (fst y) ) br]
sortPoints coords midpoints tr tl bl br =
  sortPoints (tail coords) midpoints (cmp coords midpoints tr
                                      (\ caar cadar midx midy ->
                                         caar >= midx && dadar >= midy) )
  (cmp tl)
  (cmp bl)
  (cmp br)

  -- untested code trying to port lisp version
cmp coords midpoints corner lamfun
  |  = ((head coords) ++ corner)
  | otherwise = corner



doit :: [(Int, Int)] -> IO()
doit coords = do
  print ("boundaries", (findBoundaries coords),
         "midpoints", midpoints (findBoundaries coords),
         "all points", coords,
         "sorted points", (
            sortPoints coords
            (midpoints (findBoundaries coords) )
            [] [] [] [] ))

main :: IO()
main = do
  nlines <- getLine
  inputs <- replicateM (read  nlines :: Int) getLine
  doit (tuplify inputs)
