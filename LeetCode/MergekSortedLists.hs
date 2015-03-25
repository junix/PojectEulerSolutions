{-
 Merge k sorted linked lists and return it as one sorted 
 list. Analyze and describe its complexity.
-}

update [] s = s
update x ([],[]) = (x,[])
update curr@(x:_) (xmin@(h:_),xleft)
  | x < h     = (curr,xmin:xleft)
  | otherwise = (xmin,curr:xleft)

selectMin = foldr update ([],[])

mergek :: [[Integer]] -> [Integer]
mergek [] = []
mergek xss = 
  let (xmin:xs,xleft) = selectMin xss
      other = if null xs then xleft else xs:xleft
  in  xmin : mergek other

