import Control.Monad
-- import Debug.Trace

solveMe :: [Int] -> [Int]
solveMe n
  | length n == 1 = n
  | otherwise     = tail n

intToStr :: Int -> [Char]
intToStr n = show n

strToInts :: String -> [Int]
strToInts s = map (read . (:"")) s

prepareArgs :: [Char] -> Int -> [Int]
prepareArgs n k =  (concat $ replicate k (strToInts n) )

main :: IO ()
main = do
    contents <- getContents -- read data from the stream
    let nk = (words contents) -- split by 1 space
    -- assign args, reading second arg as integer using read
    let myargs = prepareArgs (head nk) (read ( head (tail nk)))
    let res = solveMe myargs
    print res
