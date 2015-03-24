{-
 Given a string S, find the longest palindromic substring in S. 
 You may assume that the maximum length of S is 1000, 
 and there exists one unique longest palindromic substring.
-}

isPali []  = True
isPali [x] = True
isPali (x:xs) = let t   = last xs
                    mid = init xs
                in  if x /= t then False else isPali mid 

merge (maxs,acc) x = if isPali (x:acc) then (x:acc,x:acc) else (maxs,x:acc)
newPali s = fst $ foldl merge ("","") s

pali []       = []
pali s@(x:xs) = 
  let npali = newPali s
      tpali = pali xs
  in  if length npali > length tpali then 
         npali 
      else 
         tpali
