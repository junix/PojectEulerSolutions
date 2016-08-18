module Euler.P204 where
import Math.NumberTheory.Primes

ps = takeWhile (<=100) primes

es = zip ps . map (\x -> truncate . logBase (fromInteger x) $ (10^9)) $ ps

vss = map (\(v,e) -> map (v^) [0..e]) es

calc :: [[Integer]] -> [Integer]
calc [] = [1]
calc (xs:xss) = [v | p <- ps, x <- xs, let v = p * x, v <= 10^9 ]
    where ps = calc xss

euler = length . calc $ vss
