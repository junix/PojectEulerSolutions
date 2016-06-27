{-
Given two words (start and end), and a dictionary, find the length of 
shortest transformation sequence from start to end, such that:
• Only one letter can be change data time
• Each intermediate word must exist in the dictionary
For example, Given:
  start = "hit"
    end = "cog"
      dict = ["hot","dot","dog","lot","log"]
      As one shortest transformation is 
      ”hit” -> ”hot” -> ”dot” -> ”dog” -> ”cog”, return its length 5. Note:
      • Return 0 if there is no such transformation sequence.
      • All words have the same length.
      • All words containonly lower case alphabetic characters.
-}

import qualified Data.List as L

isNeighbor :: String -> String -> Bool
isNeighbor [] _  = False
isNeighbor _ []  = False
isNeighbor (x1:xs1) (x2:xs2) 
  | x1 /= x2  = xs1 == xs2
  | otherwise = isNeighbor xs1 xs2

selectOne :: [String] -> [(String,[String])]
selectOne = selectOne' []

selectOne'  acc [] = []
selectOne'  acc (x:xs) = (x,acc++xs) : selectOne' (x:acc) xs

type ConState = ([String] ,String , String ,[String])
cons :: ConState -> [ConState]
cons (acc,s,e,dict) = 
   let (ne,un) = L.partition (isNeighbor s) dict
       set = selectOne ne
   in [(s:acc,se,e,nse ++ un)|(se,nse) <- set]

calc :: [ConState] -> [[String]]
calc xs = 
  let reachSet = [reverse $ e:s:acc | (acc,s,e,_) <- xs , isNeighbor s e]
      found = not.null $ reachSet
  in if found then reachSet else concatMap (calc.cons) xs

compute s e dict = calc [([],s,e,dict)]
