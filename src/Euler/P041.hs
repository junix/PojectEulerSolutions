module Euler.P041 where
import Math.NumberTheory.Primes
import Data.List (sort)

euler41 = maximum . filter isPerm . takeWhile (<10^8) $ primes
    where isPerm xs = let s = show xs
                      in  sort s == take (length s) ['1'..'9']

