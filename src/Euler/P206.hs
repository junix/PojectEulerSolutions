module Euler.P206 where

pseq = map ((+70).(100*)) [10101010..23890266]

match n = (=="1234567890"). concat . zipWith (\x c -> if x then [c] else []) (cycle [True,False]) . show $ n

euler = head . filter (match.(^2)) $ pseq