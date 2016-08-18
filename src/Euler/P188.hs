module Euler.P188 where

modPow b 0 m = 1
modPow b 1 m = b `rem` m
modPow b e m = (v*v*v') `rem` m
    where (q, r) = e `quotRem` 2
          v      = modPow b q m
          v'     = modPow b r m

modOne x m = go x 1
    where go 1 n = n
          go v n = go v' (n+1)
            where v' = (v * x) `rem` m

modHyper x 1 m = x `rem` m
modHyper x e m = modPow x r m
    where m' = modOne x m
          r  = modHyper x (e-1) m'

euler = modHyper 1777 1855 (10^8)