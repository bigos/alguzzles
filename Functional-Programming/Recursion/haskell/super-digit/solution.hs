import Control.Monad
-- import Debug.Trace



main :: IO ()
main = do
  nk <- readLn :: IO Int
  n = first nk
  k = second nk
  let solution = solve n k
  mapM_ print solution
