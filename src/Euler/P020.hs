module Euler.P020 where

euler20 = sum.map (read.(:[])).show.product $ [2..100]

