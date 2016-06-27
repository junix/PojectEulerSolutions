{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`rem` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits  n
   | n <= 0 = []
   | otherwise = x : xs
   where x = lastDigit n
         xs = toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1,2])

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toRevDigits


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (==0). lastDigit . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n _ _ _
    | n <= 0    = []
hanoi 1 a b _   = [(a, b)]
hanoi n a b c   = f ++ [(a, b)] ++ s
    where
        f = hanoi (n-1) a c b
        s = hanoi (n-1) c b a
