module Euler.P053 where

prod 0 = 1
prod n = product [1..n]

c n r = prod n `quot` (prod r * prod (n-r))

euler = length [(n,r)| n <- [1..100], r <- [1..n], let cnr = c n r , cnr > 10^6]