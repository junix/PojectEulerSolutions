module Euler.P019 where

isLeapYear y
    | y `rem` 400 == 0 = True
    | y `rem` 4 == 0 =  y `rem` 100 /= 0
    | otherwise = False

mdays y n
    | n == 2 = if isLeapYear y then 29 else 28
    | n == 4 || n == 6 ||
      n == 9 || n == 11 = 30
    | otherwise = 31

ydays y = days y 12 31

days y m d = sum (d : map (mdays y) [1..(m-1)])

daysFrom1900 y m d = (sum . map ydays) [1900..(y-1)] + days y m d

sun y m d = (daysFrom1900 y m d) `rem` 7


euler = length [(y,m,1) | y <- [1901..2000], m <- [1..12], sun y m 1 == 0]