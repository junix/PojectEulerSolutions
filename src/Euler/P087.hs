module Euler.P087 where
import Math.NumberTheory.Primes
import Data.List (nub, delete)
import qualified Data.Set as S

bound = 50000000
pseq n = takeWhile ((<bound).(^n)) primes

euler = S.size . S.fromList $
    [s2 | z <- pseq 4
        , y <- pseq 3
        , let s1 = z^4+y^3
        , s1 < bound
        , s2 <- takeWhile (<bound) [s1 + x^2 | x <- primes]
    ]
