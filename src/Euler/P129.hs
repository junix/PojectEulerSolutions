module Euler.P129 where
import Math.NumberTheory.Primes

a n = go 1 1
    where go x c = if x `rem` n == 0
                     then c
                     else go ((x*10+1) `rem` n) (c+1)

pseq = filterP baseSeq
    where baseSeq = map (\x -> (a x, x)) . filter (\x -> gcd 10 x == 1) $ [2..]
          filterP ((a,n):xs) =
              (a,n) : filterP [ v | v@(a',n') <- xs, a' /= a ]


euler = head . filter ((>l).a)  . filter ((==1).gcd 10) $ [l..]
    where l = 1000000
