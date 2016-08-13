import Control.Monad

elat x a = head $ drop x a

inds :: Int -> [(Int, Int)]
inds n = [(x,y) | x <- [0..nn], y <- [0..nn], x < y]
  where nn = n-1


solution n k a = length $ filter (\x -> x == 0) ij
  where ij = map (\x -> mod (addij x) k) (inds n)
        addij x = (elat (fst x) a + elat (snd x) a)

splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

main :: IO()
main = do
  nk0 <- getLine
  aa0 <- getLine
  let nk = splitAndRead nk0
  let aa = splitAndRead aa0
  print $ solution (head nk) (head $ tail nk) aa
