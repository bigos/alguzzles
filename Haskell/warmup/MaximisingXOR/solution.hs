import Data.Bits
import Control.Monad

xorVal :: (Int, Int) -> Int
xorVal args = xor (fst args) (snd args)

argEnds l r =
  (map (\x ->  [x..(last [l..r] )]) [l..r])

argStarts l r =
  map (\x ->(take (length x) (repeat (head x))) ) (argEnds l r)

argPairs l r = zip
               (join (argStarts l r))
               (join (argEnds l r))

maxXor :: Int -> Int -> Int
maxXor l r =  maximum (map xorVal (argPairs l r))

-- copied from elsewhere
-- maxXor l r = maximum [x `xor` y | x <- [l, l+1.. r], y <- [l, l+1.. r]]

main :: IO ()
main = do
  l <- readLn
  r <- readLn
  print $ maxXor l r
