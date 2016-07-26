module Euler.P077 where
import Math.NumberTheory.Primes
import Data.List (sort)

calcDivs :: Integer -> [Integer] -> [[Integer]]
calcDivs 0 divs = [[]]
calcDivs n []   = []
calcDivs n (d:ds)
    | n < d     = []
    | otherwise = [ replicate cc d ++ r | c <- [0..q], let cc = fromInteger c,  r <- calcDivs (n-c*d) ds]
    where q = n `quot` d

calc n = sort . map reverse . calcDivs n $ ps
    where ps = takeWhile (<n) primes

euler = filter ((>5000).length.calc) $ [4..]