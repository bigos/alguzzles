import Control.Monad
-- import Debug.Trace

solveMe :: [Char] -> [Char]
solveMe n = n

intToStr :: Int -> [Char]
intToStr n = show n

strToInts :: String -> [Int]
strToInts s = map (read . (:"")) s

prepareArgs :: [Char] -> Int -> [Char]
prepareArgs n k =  (concat $ replicate k n )

main :: IO ()
main = do
    contents <- getContents -- read data from the stream
    let nk = (words contents) -- split by 1 space
    let myargs = prepareArgs (head nk) (read ( head (tail nk)))
    let res = solveMe myargs
    print res
