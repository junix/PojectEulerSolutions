{-
  (*) Find the last element of a list.
  (Note that the Lisp transcription of this problem is incorrect.)
  Example in Haskell:
  > myLast [1,2,3,4]
  4
  > myLast ['x','y','z']
  'z'
-}

myLast :: [a] -> a
myLast = last

myLast' :: [a] -> a
myLast' [x]    = x
myLast' (_:xs) = myLast' xs


myLast'' :: [a] -> a
myLast'' = foldl1 (\acc e -> e)

myLast''' xs = case xs of
               []      -> error "empty List"
               [x]     -> x
               (_:xs') -> myLast''' xs'

myLast'''' = foldr1 (\x acc -> acc)
             
myLastV5 = head . reverse

myLastV6 = foldr1 (const id)

myLastV7 = foldr1 (flip const)

myLastV8 = foldl1 (curry snd)

myLastV9 xs = xs !! (length xs - 1)
