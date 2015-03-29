{-
    Given a digit string, return all possible letter 
    combinations that the number could represent.
    A mapping of digit to letters (just like on the 
    telephone buttons) is given below.

    Phone Keyboard
    Input:Digit string ”23”
    Output: [”ad”, ”ae”, ”af”, ”bd”, ”be”, ”bf”, ”cd”, 
             ”ce”, ”cf”].
    Note: Although the above answer is in lexicographical 
    order, your answer could be in any order you want.
-}

digit2str '2' = "abc"
digit2str '3' = "def"
digit2str '4' = "ghi"
digit2str '5' = "jkl"
digit2str '6' = "mno"
digit2str '7' = "pqrs"
digit2str '8' = "tuv"
digit2str '9' = "wxyz"

keyboard :: String -> [String]
keyboard ""     = []
keyboard (x:[]) = [v:[]| v <- digit2str x]
keyboard (x:xs) = [v:l | v <- digit2str x
                       , l <- keyboard xs]

