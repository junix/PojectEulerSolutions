{-
  Given a linked list, remove the nth node from the end 
  of list and return its head.  For example,
  Given linked list: 1->2->3->4->5, and n = 2.After removing 
  the second node from the end, the linked list becomes 1->2->3->5.

  Note:
    Given n will always be valid.
    Try to do this in one pass.
-}

rrem :: (Eq a) => [a] -> Int -> [a]
rrem xs n = 
  let (h,t)     = splitAt n xs
      (acc,buf) = foldl update' ([],reverse h) t
  in  reverse acc ++ (reverse.init $ buf)
  where 
    update' (acc,buf) e = (last buf:acc,e : init buf)
