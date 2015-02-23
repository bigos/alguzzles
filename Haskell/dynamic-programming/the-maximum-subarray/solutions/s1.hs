import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

processInput :: Int -> (Int, Int, Int) -> (Int, Int, Int)
processInput arrVal (bestSum, curSum, nonCont) =
   let newCurSum = max (arrVal + curSum) $! 0
       newBestSum = max bestSum $! newCurSum
       nonContAcc = max arrVal $! 0
    in (newBestSum, newCurSum, nonCont + nonContAcc)

maxBoth :: [Int] -> (Int, Int)
maxBoth arr = (if maxContiguous /= 0 then maxContiguous else maximum arr,
               if maxNonContiguous /= 0 then maxNonContiguous else maximum arr)
              where (maxContiguous, _, maxNonContiguous) =
                     foldr processInput (0, 0, 0) arr

filterOdd :: [ByteString] -> [ByteString]
filterOdd (_:o:xs) = o:filterOdd xs
filterOdd [] = []
filterOdd [_] = []

arraysLine :: [Int] -> String
arraysLine array = show maxContiguous ++ " " ++ show maxNonContiguous
  where (maxContiguous, maxNonContiguous) = maxBoth array

bsReadInt :: ByteString -> Int
bsReadInt str = case BS.readInt str of
                     Just (val, _) -> val
                     Nothing -> undefined

main :: IO ()
main = do
  _ <- getLine
  contents <- BS.getContents
  mapM_ (putStrLn . arraysLine . map bsReadInt . BS.words) $! (filterOdd . BS.lines) contents
