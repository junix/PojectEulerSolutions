module Euler.P009 where
{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

calc = head [x*y*z | x <- [1..1000]
                   , y <- [x..1000]
                   , z <- [1000-x-y]
                   , z > y
                   , x^2 + y^2 == z^2 ]
