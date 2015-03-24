{-
 Given a string S, find the longest palindromic substring in S. 
 You may assume that the maximum length of S is 1000, 
 and there exists one unique longest palindromic substring.
-}

isPali xs = xs == reverse xs

merge (maxs,acc) x = let acc'  = x:acc 
                         maxs' = if isPali acc' then acc' else maxs
                     in (maxs',acc')

tailPali s = fst $ foldl merge ("","") s

pali []       = []
pali s@(x:xs) = 
  let npali = tailPali s
      tpali = pali xs
  in  if length npali > length tpali then 
         npali 
      else 
         tpali
