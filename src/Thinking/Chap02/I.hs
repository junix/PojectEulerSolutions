module Thinking.Chap02.I where

import Data.Char

palindrome :: IO ()
palindrome = do
    putStrLn "Enter a string:"
    line <- getLine
    if isPalindrome line
    then putStrLn "yes!"
    else putStrLn "no!"

isPalindrome :: [Char] -> Bool
isPalindrome xs =
  let
        trim = (map toLower) . (filter isAlphaNum) $ xs
  in
        trim == reverse trim

a `hello` b = a < b

data H = H Int | S String

