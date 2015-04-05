{-
Given two words (start and end), and a dictionary, find the length of shortest transformation sequence from start to end, such that:
• Onlyonelettercanbechangedatatime
• Eachintermediatewordmustexistinthedictionary
For example, Given:
  start = "hit"
    end = "cog"
      dict = ["hot","dot","dog","lot","log"]
      As one shortest transformation is ”hit” -> ”hot” -> ”dot” -> ”dog” -> ”cog”, return its length 5. Note:
      • Return0ifthereisnosuchtransformationsequence.
      • Allwordshavethesamelength.
      • Allwordscontainonlylowercasealphabeticcharacters.
-}

isNeighbor :: String -> String -> Bool
isNeighbor [] _  = False
isNeighbor _ []  = False
isNeighbor (x1:xs1) (x2:xs2) 
  | x1 /= x2 = xs1 == xs2
  | otherwise = isNeighbor xs1 xs2

allNeighbors :: String -> [String] -> ([String],[String])
allNeighbors curr = foldr update ([],[])
   where update x (ne,un) = if isNeighbor curr x 
                            then (x:ne,un) 
                            else (ne,x:un)

selectOne :: [String] -> [(String,[String])]
selectOne = selectOne' []

selectOne'  acc [] = []
selectOne'  acc (x:xs) = (x,acc++xs) : selectOne' (x:acc) xs

type ConState = ([String] ,String , String ,[String])
cons :: ConState -> [ConState]
cons (acc,s,e,dict) = let (ne,un) = allNeighbors s dict
                          seset = selectOne ne
                       in [(s:acc,se,e,nse ++ un)|(se,nse) <- seset]

calc :: [ConState] -> [[String]]
calc xs = let founded = [reverse $ e:s:acc | (acc,s,e,_) <- xs , isNeighbor s  e]
          in if not.null $ founded 
             then founded
             else [v | (acc,s,e,dict) <- xs , 
                       v <- if null dict then [] else calc (cons (acc,s,e,dict))]

compute s e dict = calc [([],s,e,dict)]
