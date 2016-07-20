module Euler.P028 where

endOf n = n * n

startOf 0 = 1
startOf n = endOf (n-2) + 1

select 1 = 1
select n = sum . zipWith (\x y-> if x `rem` (n-1) == 0 then y else 0)  [1..] $ [startOf n..endOf n]

euler n = sum . map select $ [1,3..n]

