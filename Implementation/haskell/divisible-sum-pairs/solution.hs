import Control.Monad


splitAndRead :: String -> [Int]
splitAndRead str = map read (words str)

main :: IO()
main = do
  nk0 <- getLine
  aa0 <- getLine
  let nk = splitAndRead nk0
  let aa = splitAndRead aa0
  print [nk, aa]
