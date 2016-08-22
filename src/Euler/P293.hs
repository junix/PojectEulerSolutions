module Euler.P293 where
import Math.NumberTheory.Primes
import Data.List (nub)

cons limit = go 0 1
    where go c acc (y:ys) = [acc | c > 1] ++ [ v  | e <- yes, v <- go (c+1) e ys ]
             where yes = takeWhile (<limit) . map ((acc*).(y^)) $ [1..]

exp2 limit = takeWhile (<limit) . map (2^) $ [1..]

admissibleNumbers limit = exp2 limit ++ cons limit primes

fortunateNumber n = subtract n . head . filter isPrime $ [n+2..]

euler = sum . nub . map fortunateNumber . admissibleNumbers $  (10^9)