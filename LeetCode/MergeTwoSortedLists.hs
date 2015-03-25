{-
 Merge two sorted linked lists and return it as a new list. 
 The new list should be made by splicing together the nodes 
 of the first two lists.
-}

merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge l@(x1:xs1) r@(x2:xs2)
  | x1 < x2   = x1:merge xs1 r
  | otherwise = x2:merge l xs2
