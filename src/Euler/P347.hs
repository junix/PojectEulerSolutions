module Euler.P347 where
import Math.NumberTheory.Primes


cons limit (x:xs) = concatMap go es
    where es = takeWhile (<limit) . map (x^) $ [1..]
          go e = [ maximum vs | y <- takeWhile ((<limit).(e*)) xs
                     , let vs = 0:(takeWhile (<limit) . map ((e*).(y^)) $ [1..])
                     ]

s limit = go primes
    where go xs'@(x:xs) = if null cs then [] else maximum cs : go xs
            where cs = cons limit xs'