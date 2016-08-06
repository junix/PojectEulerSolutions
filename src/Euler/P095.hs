module Euler.P095 where
import Math.NumberTheory.Primes
import qualified Data.Set as S

divs n = sum . init . S.toList . divisors $ n

chain n = go n 0 S.empty
    where go 0 c d = 0
          go x c d | x > 1000000000 = 0
          go x c d = if x' == n
                        then c + 1
                        else if S.member x' d
                                then 0
                                else go x' (c+1) (S.insert x' d)
            where x' = divs x

euler = head . filter ((>27).fst) . map (\x -> (chain x, x)) $ [28..1000000]
