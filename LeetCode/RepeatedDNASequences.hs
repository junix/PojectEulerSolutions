{-
 All DNA is composed of a series of nucleotides abbreviated 
 as A, C, G, and T, for example: "ACGAATTCCG". When studying 
 DNA, it is sometimes useful to identify repeated sequences 
 within the DNA.

 Write a function to find all the 10-letter-long sequences 
 (substrings) that occur more than once in a DNA molecule.

 For example,

 Given s = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT",

 Return:
 ["AAAAACCCCC", "CCCCCAAAAA"].
-}

import qualified Data.Map as M

update' (dict,last9Ch) e =
  let key  = e:last9Ch
      nchs = init key
      cnt  = case M.lookup key dict of 
               Just n -> n
               _      -> 0
  in  (M.insert key (cnt+1) dict, nchs)
                                        

findRe :: String -> [String]
findRe str = let (hd,tl) = splitAt 9 str 
             in  M.keys . M.filter (>1) . fst . foldl update' (M.empty,reverse hd) $ tl
                                        
