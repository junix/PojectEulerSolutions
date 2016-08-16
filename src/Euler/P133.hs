module Euler.P133 where
import Math.NumberTheory.Primes
import qualified Data.Set as S
import Data.List ((\\))

modPow b 0 m = 1
modPow b 1 m = b `rem` m
modPow b e m = (v*v*v') `rem` m
    where (q, r) = e `quotRem` 2
          v      = modPow b q m
          v'     = modPow b r m

goModPow m = go 1 (modPow 10 10 (9*m))
    where go n 1 = True
          go 100 _ = False
          go c r = go (c+1) ((r^10) `rem` (9*m))


ps = S.fromList . takeWhile (<10^5) $ primes

ps' = takeWhile (<10^5) $ primes

canDivide x n = modPow 10 (10^(n+1)) (x*9) == 1

canMod x = any (canDivide x) [1..100]

factorsOf n set = S.fromList . filter canDivide . S.toList $ set
        where canDivide x = modPow 10 (10^(n+1)) (x*9) == 1

factors = go 1 ps S.empty
    where go n xs set = if set == set'
                        then go (n+1) xs' set'
                        else (length set',n) : go (n+1) xs' set'
            where ds   = factorsOf n xs
                  set' = S.union set ds
                  xs'  = xs S.\\ ds

e = filter (canMod)  ps'
