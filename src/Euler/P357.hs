module Euler.P357 where
import Math.NumberTheory.Primes

isGen n = exps && go facts
    where factors = factorise n
          exps = all (==1) . map snd $ factors
          facts = map fst factors
          go [] = True
          go (x:xs) = isPrime (x + (n `quot` x)) && go xs



pseq = filter isGen . tail . map (subtract 1) . takeWhile (<10^8) $ primes



