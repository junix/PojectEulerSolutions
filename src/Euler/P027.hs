module Euler.P027 where
import Math.NumberTheory.Primes

formula a b = length . takeWhile isPrime . map (\x -> x*x + a*x + b) $ [0..]

euler = maximum [ (formula a b, a*b) | a<-[-1000..1000], b<- takeWhile (<1000) primes]