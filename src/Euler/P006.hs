module Euler.P006 where
import Control.Monad
{-
The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-}

-- reader monad
calc :: [Integer] -> Integer
calc = do
    x <- (^2) . sum
    y <- sum . map (^2)
    return (x-y)
