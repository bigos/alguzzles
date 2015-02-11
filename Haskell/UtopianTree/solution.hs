import Control.Monad

-- run it in terminal
-- $ cat ./input1.txt | runhaskell solution.hs

calcGrowth :: Int -> Int
calcGrowth 0 = 1
calcGrowth 1 = 2
calcGrowth x = (x * 2) + (x * 1) -- not working yet
-- working Lisp example
-- (defun calc (i)
--   (loop for x from 0 to i
--      for total = 1 then (if (oddp x)
--                             (* total 2)
--                             (+ 1 total))
--      finally (return total)))

main :: IO ()
main = do
  t <- readLn :: IO Int
  strs <- replicateM t getLine
  let cycles = map read strs :: [Int]
  print t
  print cycles
  let answers = map calcGrowth cycles
  forM_ answers print
  -- this gives same alues astheLisp loop
  print  (join [ [2 ^ x - 2 ,2 ^ x - 1] | x <- [1..5] ])
