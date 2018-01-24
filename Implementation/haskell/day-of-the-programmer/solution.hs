-- day of the programmer

divisible n d = (mod n d) == 0

julianLeap y = divisible y 4

georgianLeap y = a || b
  where a = divisible y 400
        b = (divisible y 4) && not (divisible y 100)

leapYear y
  | y <= 1917 = julianLeap y
  | y == 1918 = False
  | y >= 1919 = georgianLeap y

solve y
  | y == 1918 = "26.09.1918"
  | otherwise = if leapYear y then "12.09." else "13.09." ++ show y

inputYear :: IO Int
inputYear = getLine >>= return . read

main = inputYear >>= putStrLn . solve
