{-
Given a collection of numbers, return all possible permutations.

For example,
[1,2,3] have the following permutations:
[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], and [3,2,1].
-}

insert' n x xs = let (h,t) = splitAt n xs in h ++ [x] ++ t

perm :: [a] -> [[a]]
perm [] = []
perm [x] = [[x]]
perm (x:xs) = let rperms = perm xs
                  len = length xs
                  inserts = map (\n->insert' n x) [0..len]
              in [f x | f<-inserts, x <- rperms]
