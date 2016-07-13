{-

(*) Find the last but one element of a list.
(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:

Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'

-}


 myButLast :: [a] -> a
 myButLast [] = error "empty list"
 myButLast [x] = error "only one element"
 myButLast (a:b:[]) = a
 myButLast (_:xs) = myButLast xs


 myButLastV2 xs = xs !! (length xs - 2)

 myButLastV3 = head .drop 1. reverse

 myButLastV4 = last.init
