{-
 Given an array of size n, find the majority element. 
 The majority element is the element that appears more 
 than ⌊ n/2 ⌋ times.You may assume that the array is non-empty 
 and the majority element always exist in the array.
-}

majorityElem xs = 
  fst $ foldl update (0,0) xs 
  where 
    update (_,0) x = (x,1)
    update (v,c) x = (v,c + if x == v then 1 else (-1))
