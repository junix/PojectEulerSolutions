{-
Implement next permutation, which rearranges numbers into the lexicographically next greater permutation of numbers.

If such arrangement is not possible, it must rearrange it as the lowest possible order (ie, sorted in ascending order).

The replacement must be in-place, do not allocate extra memory.

Here are some examples. Inputs are in the left-hand column and its corresponding outputs are in the right-hand column.
1,2,3 → 1,3,2
3,2,1 → 1,2,3
1,1,5 → 1,5,1
-}

spanCmp :: [Integer] -> ([Integer],[Integer])
spanCmp [] = ([],[])
spanCmp [x] = ([],[x])
spanCmp (x:y:xs)
  | x > y = ([x],y:xs)
  | otherwise = let (a,b) = spanCmp (y:xs) in  (x:a,b)

nextPerm :: ([Integer],[Integer]) -> [Integer]
nextPerm (hd,x:xs) = let (h,t@(th:txs)) = span (<x) hd
                     in h++reverse ([x]++txs) ++ [th] ++ xs

nperm :: [Integer] -> [Integer]
nperm xs = reverse.nextPerm.spanCmp.reverse $ xs
