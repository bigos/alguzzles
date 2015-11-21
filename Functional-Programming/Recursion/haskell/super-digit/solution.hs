import Control.Monad
-- import Debug.Trace

-- not working



main = do
    contents <- getContents
    let nk = map read (words contents) :: [Int]
    print nk
