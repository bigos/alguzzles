import Control.Monad
-- import Debug.Trace

solution :: Int -> Int -> (Int, Int)
solution n k = (n, k)

main :: IO ()
main = do
    contents <- getContents
    let nk = map read (words contents) :: [Int]
    print (  solution (head nk) (head(tail nk)))
