{-
(*) Find the K'th element of a list. The first element in the list is number 1.
Example:

(element-at '(a b c d e) 3)
c

Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'

-}

elementAt :: [a] -> Int -> a
--elementAt [] _ = error "empty list"
elementAt (x:_) 0 = x
elementAt (_:xs) n = elementAt xs (n-1)

elementAt' xs 0 = head xs
elementAt' xs n = head . drop n $ xs 
