module Euler.P051 where
import Math.NumberTheory.Primes

genPattern  = init . tail . go
    where go :: String -> [String]
          go []  = [[]]
          go (x:xs) = [h:t | h <- [x,'*'], t <- go xs]

repCh n '*' = n
repCh n x = x

repStr :: String -> Char -> String
repStr s n = map (repCh n) s

genNum :: String -> [Integer]
genNum p@('*':xs) = map (read.(repStr p)) $ ['1'..'9']
genNum pattern =  map (read.(repStr pattern)) $ ['0'..'9']

euler :: Integer
euler =head [head x| (n,patterns)<- map (\x -> (x, (genPattern . show) x)) $ primes
                   , p <- patterns
                   , let x = filter isPrime . genNum $ p
                   , length x == 8 ]
