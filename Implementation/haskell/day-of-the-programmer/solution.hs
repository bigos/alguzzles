-- day of the programmer

zerop :: Int -> Bool
zerop 0 = True
zerop _ = False

divisible n d = zerop (mod n d)

julianLeap y = divisible y 4

georgianLeap y = a || b
  where a = divisible y 400
        b = (divisible y 4) && not (divisible y 100)

inputYear :: IO Int
inputYear = getLine >>= return . read

leapYear y
  | y <= 1917 = julianLeap y
  | y == 1918 = False
  | y >= 1919 = georgianLeap y

ys :: Int -> String
ys y = show y

solve y
  | y == 1918 = "26.09.1918"
  | otherwise = if (leapYear y) then "12.09." ++ ys y else "13.09." ++ ys y

main = inputYear >>= putStrLn . solve
