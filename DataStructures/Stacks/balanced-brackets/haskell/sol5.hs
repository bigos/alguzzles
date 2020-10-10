import Control.Monad (forM_)

brac :: Char -> Either Int Int
brac '(' = Left  1
brac ')' = Right 1
brac '[' = Left  2
brac ']' = Right 2
brac '{' = Left  3
brac '}' = Right 3
brac c   = error $ "Unexpected char: " ++ [c]

balanced :: String -> Bool
balanced = balanced' [] . map brac
  where
    balanced' :: [Int] -> [Either Int Int] -> Bool
    balanced' ms      []            = null ms
    balanced' ms     ((Left x) :cs) = balanced' (x:ms) cs
    balanced' []     ((Right x):cs) = False
    balanced' (m:ms) ((Right x):cs) = x == m && balanced' ms cs

main :: IO ()
main = do n <- readLn
          forM_ [1..n] (const $
            do str <- getLine
               if balanced str then
                 putStrLn "YES"
               else
                 putStrLn "NO")
